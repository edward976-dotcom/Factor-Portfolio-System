library(shiny)
library(tidyquant)
library(tidyverse)
library(bslib)
library(zoo)

# ==========================================
# 1. UI 前端介面設計 (支援 N 檔標的與因子選股)
# ==========================================
ui <- page_navbar(
  title = "投資組合與因子選股系統",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  # === 分頁 1: 投資組合最佳化 ===
  nav_panel(
    title = "📊 投資組合最佳化 (Portfolio Optimization)",
    icon = icon("chart-pie"),
    
    page_sidebar(
      sidebar = sidebar(
        width = 350,
        h4("⚙️ 標的與參數設定", style = "margin-bottom: 15px; font-weight: bold;"),
        
        selectizeInput("assets", "投資組合名單 (輸入多檔代碼)", 
                       choices = c("2330.TW" = "2330.TW", "2049.TW" = "2049.TW", "2308.TW" = "2308.TW", "2881.TW" = "2881.TW"), 
                       selected = c("2330.TW", "2049.TW", "2308.TW", "2881.TW"), 
                       multiple = TRUE,
                       options = list(create = TRUE, placeholder = "例如: AAPL, MSFT...")),
        
        dateRangeInput("date_range", "回測期間", 
                       start = "2020-01-01", end = Sys.Date()),
        
        textInput("benchmark", "大盤對沖基準 (Benchmark)", value = "^TWII"),
        
        numericInput("rf_rate", "無風險利率 (年化, %)", value = 2, min = 0, step = 0.5),
        
        selectInput("roll_months", "滾動相關性視窗 (月)", 
                    choices = c("3 個月" = 3, "6 個月" = 6, "12 個月" = 12), selected = 6),
        
        hr(),
        
        h4("⚖️ 權重配置 (自動歸一化為 100%)", style = "margin-top: 10px; font-weight: bold;"),
        uiOutput("sliders_ui"),
        textOutput("weight_sum_warning"),
        
        # 最佳化建議權重
        uiOutput("optimal_weights_ui"),
        
        hr(),
        actionButton("go_button", "開始運算 / 更新模型", 
                     class = "btn-primary btn-lg", style = "width: 100%; font-weight: bold;"),
        downloadButton("dl_report", "⬇️ 下載權重與績效報告 (CSV)", 
                       class = "btn-secondary btn-sm", style = "width: 100%; margin-top: 10px;")
      ),
      
      layout_columns(
        col_widths = 12,
        fill = FALSE,         # 取消強迫填滿容器，讓原本的 height 設定能生效
        
        # 效能表格全寬置頂
        card(
          card_header("📊 投資組合風險與報酬對決 (Performance Metrics)", class = "bg-primary text-white"),
          tableOutput("perfTable")
        ),
        
        # 累積財富：放大並獨立一行
        layout_columns(
          col_widths = c(8, 4),
          card(
            full_screen = TRUE, # 允許點擊展開全螢幕
            card_header("📈 累積資產成長曲線 vs 大盤 (Cumulative Wealth)"),
            plotOutput("wealthPlot", height = "500px")
          ),
          card(
            full_screen = TRUE,
            card_header("⚖️ 各標的風險貢獻度 (Risk Contribution)"),
            plotOutput("riskContribPlot", height = "500px")
          )
        ),
        
        # 下半部切分左右，或如果覺得太擠也可以全部獨立 (這裡保持上下獨立讓細節更清楚)
        card(
          full_screen = TRUE,
          card_header("📐 馬可維茲效率前緣與隨機模擬 (Frontier Cloud)"),
          plotOutput("frontierPlot", height = "600px"),
          helpText("灰點：一萬組隨機權重投組；紅點：最高夏普比率；藍點：最低波動度；橘點：目前選擇的配置。", style = "text-align: center; color: gray;")
        ),
        
        card(
          full_screen = TRUE,
          card_header("🔄 資產間動態相關係數熱圖 (Rolling Correlation Network)"),
          plotOutput("corPlot", height = "600px")
        )
      )
    )
  ),
  
  # === 分頁 2: 多因子選股 ===
  nav_panel(
    title = "🔍 多因子選股掃描器 (Factor Screener)",
    icon = icon("filter"),
    
    page_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("⚙️ 篩選池設定", style = "margin-bottom: 20px; font-weight: bold;"),
        textAreaInput("screener_universe", "輸入觀察清單 (逗號分隔)", 
                      value = "AAPL, MSFT, GOOGL, AMZN, NVDA, META, TSLA, JPM, V, WMT, JNJ, PG", 
                      rows = 6),
        actionButton("run_screener", "開始掃描並計算因子", class = "btn-success", style = "width: 100%;"),
        hr(),
        helpText("預設計算：\n- 動能 (Momentum)\n- 波動度 (Volatility)\n- 夏普比率 (Sharpe)")
      ),
      card(
        card_header("🏆 因子排名榜 (Factor Rankings)", class = "bg-success text-white"),
        tableOutput("screenerTable")
      )
    )
  )
)

# ==========================================
# 2. Server 後端邏輯運算
# ==========================================
server <- function(input, output, session) {
  
  # === 動態生成 Slider ===
  output$sliders_ui <- renderUI({
    req(input$assets)
    syms <- input$assets
    n <- length(syms)
    if (n < 2) return(helpText("請至少輸入兩檔標的進行投資組合分析。", style = "color: red;"))
    
    # 平均分配初始權重
    init_w <- round(100 / n)
    
    slider_list <- lapply(seq_along(syms), function(i) {
      val <- if(i == n) 100 - init_w * (n - 1) else init_w
      sliderInput(paste0("w_", i), syms[i], min = 0, max = 100, value = val, step = 1)
    })
    
    do.call(tagList, slider_list)
  })
  
  # 獲取目前使用者設定的權重向量並自動歸一化
  current_weights <- reactive({
    req(input$assets)
    syms <- input$assets
    w <- sapply(seq_along(syms), function(i) {
      val <- input[[paste0("w_", i)]]
      if(is.null(val)) 100/length(syms) else val
    })
    w / sum(w) # 確保總和恆為 1
  })
  
  output$weight_sum_warning <- renderText({
    req(input$assets)
    syms <- input$assets
    w <- sapply(seq_along(syms), function(i) input[[paste0("w_", i)]])
    if (any(is.null(w))) return("")
    s <- sum(w)
    if (s != 100) paste0("⚠️ 注意：目前權重總和為 ", s, "%，系統已自動依比例歸一化至 100%。") else ""
  })
  
  # 【響應式資料獲取】
  raw_data <- eventReactive(input$go_button, {
    req(input$assets)
    syms <- trimws(input$assets)
    validate(need(length(syms) >= 2, "請至少輸入兩檔有效的標的代碼。"))
    
    withProgress(message = '正在從 Yahoo Finance 獲取資料...', value = 0.5, {
      tryCatch({
        df <- tq_get(syms, get = "stock.prices", 
                     from = input$date_range[1], to = input$date_range[2]) %>% 
          drop_na(close)
        
        # 驗證資料筆數
        validate(need(length(unique(df$symbol)) >= 2, "錯誤：請確認輸入的股票代碼是否有效，且至少需有兩檔標的具備歷史資料。"))
        df
      }, error = function(e) {
        validate(need(FALSE, paste("下載資料發生錯誤：", e$message)))
      })
    })
  }, ignoreNULL = FALSE)
  
  # 【響應式資料獲取 - 大盤基準】
  bench_data <- eventReactive(input$go_button, {
    req(input$benchmark)
    withProgress(message = '正在獲取大盤資料...', value = 0.8, {
      tryCatch({
        df <- tq_get(trimws(input$benchmark), get = "stock.prices", 
                     from = input$date_range[1], to = input$date_range[2]) %>% drop_na(close)
        validate(need(nrow(df) > 0, "錯誤：獲取大盤基準資料失敗，請檢查代碼是否正確。"))
        df
      }, error = function(e) {
        validate(need(FALSE, paste("獲取大盤基準資料發生錯誤：", e$message)))
      })
    })
  }, ignoreNULL = FALSE)
  
  daily_bench_returns <- reactive({
    req(bench_data())
    bench_data() %>%
      arrange(date) %>%
      mutate(return = close / lag(close) - 1) %>%
      drop_na(return) %>%
      select(date, bench_ret = return)
  })
  
  # 【計算日報酬率】
  daily_returns <- reactive({
    req(raw_data())
    raw_data() %>%
      arrange(symbol, date) %>%
      group_by(symbol) %>%
      mutate(return = close / lag(close) - 1) %>%
      ungroup() %>%
      drop_na(return)
  })
  
  # 【建立報酬矩陣】供強大且秒速的矩陣運算使用
  ret_matrix_data <- reactive({
    req(daily_returns())
    wide_ret <- daily_returns() %>%
      select(date, symbol, return) %>%
      distinct(date, symbol, .keep_all = TRUE) %>% # 處理 Yahoo Finance 重複資料
      pivot_wider(names_from = symbol, values_from = return) %>%
      arrange(date) %>%
      drop_na()
    
    syms <- trimws(input$assets)
    # 確保所有輸入代碼都在寬表格中
    valid_syms <- intersect(syms, colnames(wide_ret))
    if (length(valid_syms) < 2) return(NULL)
    
    mat <- as.matrix(wide_ret[, valid_syms])
    
    # 計算年化預期報酬與共變異數矩陣
    mu <- colMeans(mat) * 252
    Sigma <- cov(mat) * 252
    
    list(dates = wide_ret$date, returns = mat, mu = mu, Sigma = Sigma, syms = valid_syms)
  })
  
  
  # 【產出 1：累積資產成長曲線】(向量化內積，極速運算，支援 N 檔與大盤對比)
  output$wealthPlot <- renderPlot({
    mat_data <- ret_matrix_data()
    req(mat_data)
    
    w_vec <- current_weights()
    req(length(w_vec) == length(mat_data$syms))
    
    wts <- matrix(w_vec, ncol = 1)
    
    # 投組每日報酬
    port_daily <- mat_data$returns %*% wts
    wealth_index <- cumprod(1 + as.numeric(port_daily))
    
    port_df <- tibble(date = mat_data$dates, wealth = wealth_index, type = "投資組合 (Portfolio)")
    
    # 加入大盤資料比對
    bench_df <- daily_bench_returns()
    bench_name <- paste0("大盤基準 (", input$benchmark, ")")
    
    if (!is.null(bench_df)) {
        comb <- tibble(date = mat_data$dates) %>%
            left_join(bench_df, by = "date") %>%
            replace_na(list(bench_ret = 0))
            
        bench_wealth <- cumprod(1 + comb$bench_ret)
        b_df <- tibble(date = comb$date, wealth = bench_wealth, type = bench_name)
        plot_df <- bind_rows(port_df, b_df)
    } else {
        plot_df <- port_df
    }

    # 動態設定顏色與線條
    color_map <- c("投資組合 (Portfolio)" = "#18bc9c")
    color_map[bench_name] <- "gray30"
    
    line_map <- c("投資組合 (Portfolio)" = "solid")
    line_map[bench_name] <- "dashed"
    
    ggplot(plot_df, aes(x = date, y = wealth, color = type, linetype = type)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 1, linetype = "dotted", color = "gray50") +
      scale_color_manual(values = color_map) +
      scale_linetype_manual(values = line_map) +
      theme_tq(base_size = 16) + # 放大基底字體
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
      ) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      labs(x = "", y = "累積倍數 (起點 = 1)")
  })
  
  
  # 【產出新增：風險貢獻度分析 (Risk Parity Analysis)】
  output$riskContribPlot <- renderPlot({
    mat_data <- ret_matrix_data()
    req(mat_data)
    
    w_vec <- current_weights()
    req(length(w_vec) == length(mat_data$syms))
    wts <- matrix(w_vec, ncol = 1)
    
    # 邊際風險與風險貢獻度 (Marginal Risk & Risk Contribution)
    Sigma <- mat_data$Sigma
    port_var <- as.numeric(t(wts) %*% Sigma %*% wts)
    port_vol <- sqrt(port_var)
    
    # Marginal Risk (MR) = (Sigma %*% w) / port_vol
    marginal_risk <- (Sigma %*% wts) / port_vol
    
    # Risk Contribution = w * MR
    risk_contrib <- wts * marginal_risk
    
    # Percentage Risk Contribution
    pct_risk_contrib <- as.numeric(risk_contrib / port_vol)
    
    df <- tibble(
      symbol = mat_data$syms,
      RC = pct_risk_contrib
    ) %>% arrange(RC) # 排序圖表
    
    df$symbol <- factor(df$symbol, levels = df$symbol)
    
    ggplot(df, aes(x = symbol, y = RC, fill = RC > 0)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db")) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = sprintf("%.1f%%", RC * 100)), 
                hjust = ifelse(df$RC > 0, -0.2, 1.2), size = 6, fontface = "bold") +
      theme_minimal(base_size = 16) +
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      labs(x = "", y = "風險貢獻佔比 (Risk Contribution %)")
  })
  
  
  # 【產出 2：動態相關係數熱圖】(針對 N 檔標的繪製最新設定月份之相關性矩陣)
  output$corPlot <- renderPlot({
    mat_data <- ret_matrix_data()
    req(mat_data)
    n <- length(mat_data$syms)
    if(n < 2) return(ggplot() + theme_void())
    
    # 對於多檔標的，畫出近期的相關矩陣熱圖
    # 將使用者選擇的月份轉為數值，並換算交易日 (1個月約 21 個交易日)
    roll_m <- as.numeric(req(input$roll_months))
    recent_n <- min(roll_m * 21, nrow(mat_data$returns))
    recent_ret <- tail(mat_data$returns, recent_n)
    cormat <- cor(recent_ret)
    
    library(reshape2)
    melted_cormat <- melt(cormat)
    ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#3498db", high = "#e74c3c", mid = "white", 
                           midpoint = 0, limit = c(-1,1), name=paste0("Correlation\n(Last ", roll_m, " Months)")) +
      geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 6, fontface = "bold") + # 放大方塊內數字
      theme_minimal(base_size = 16) + # 放大整體基底
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1, face = "bold"), # 放大 X 軸代碼標籤
        axis.text.y = element_text(size = 16, face = "bold"), # 放大 Y 軸代碼標籤
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      ) +
      coord_fixed()
  })
  
  
  # 【產出 3：馬可維茲效率前緣與最佳化計算】(純矩陣數學解 + 蒙地卡羅萬組隨機投組)
  frontier_df <- reactive({
    mat_data <- ret_matrix_data()
    req(mat_data)
    
    n <- length(mat_data$syms)
    n_sim <- 10000 # 生成一萬組隨機權重
    
    # 產生 Dirichlet 分佈隨機權重 (總和保證為 1)
    # 利用 rgamma 產生後正規化是一種快速且分布均勻的作法
    rand_w <- matrix(runif(n * n_sim), nrow = n_sim, ncol = n)
    W <- rand_w / rowSums(rand_w)
    
    # 計算 10,000 種權重的預期報酬 Return = W %*% mu
    port_ret <- W %*% matrix(mat_data$mu, ncol = 1)
    
    # 計算 10,000 種權重的預期波動度 Volatility = sqrt( diag(W %*% Sigma %*% t(W)) )
    port_vol <- sqrt(rowSums((W %*% mat_data$Sigma) * W))
    
    rf <- input$rf_rate / 100
    
    # 將每一列的權重存入字串以便查閱
    w_str <- apply(W, 1, function(rw) {
      paste0(mat_data$syms, ": ", round(rw * 100), "%", collapse = " | ")
    })
    
    tibble(
      w_matrix = I(lapply(seq_len(nrow(W)), function(i) W[i,])), # 儲存原始權重向量
      w_label = w_str,
      Return = as.numeric(port_ret),
      Volatility = as.numeric(port_vol),
      Sharpe = (Return - rf) / Volatility
    )
  })
  
  # 動態 UI 展現建議之「最大夏普」與「最小波動」權重
  output$optimal_weights_ui <- renderUI({
    df <- frontier_df()
    if(is.null(df)) return(NULL)
    
    best_sharpe <- df %>% slice_max(Sharpe, n = 1, with_ties = FALSE)
    min_vol <- df %>% slice_min(Volatility, n = 1, with_ties = FALSE)
    
    div(
      p("🏆 蒙地卡羅模擬最佳化建議：", style = "font-weight: bold; margin-bottom: 5px; font-size: 1.1em;"),
      
      div(style = "padding: 10px; background-color: #fcebeb; border-radius: 5px; margin-bottom: 10px;",
        p("🔥 最大夏普比率 (Max Sharpe)", style = "color: #e74c3c; font-weight: bold; margin-bottom: 3px;"),
        p(best_sharpe$w_label, style = "margin-bottom: 8px; font-size: 0.9em;"),
        actionButton("apply_opt", "套用: 最大夏普", class = "btn-danger btn-sm", icon = icon("fire"))
      ),
      
      div(style = "padding: 10px; background-color: #eaf2f8; border-radius: 5px; margin-bottom: 5px;",
        p("🛡️ 最低波動度 (Min Volatility)", style = "color: #2980b9; font-weight: bold; margin-bottom: 3px;"),
        p(min_vol$w_label, style = "margin-bottom: 8px; font-size: 0.9em;"),
        actionButton("apply_minvol_opt", "套用: 最低波動", class = "btn-info btn-sm", icon = icon("shield-halved"))
      )
    )
  })
  
  # 輔助函數：更新 Sliders
  update_all_sliders <- function(best_w) {
    best_w_pct <- round(best_w * 100)
    # 修補進位誤差確保總和 100
    diff <- 100 - sum(best_w_pct)
    if (diff != 0) best_w_pct[which.max(best_w_pct)] <- best_w_pct[which.max(best_w_pct)] + diff
    
    for (i in seq_along(best_w_pct)) {
      updateSliderInput(session, paste0("w_", i), value = best_w_pct[i])
    }
  }
  
  # 當點擊套用「最大夏普」
  observeEvent(input$apply_opt, {
    df <- frontier_df()
    req(df)
    best_w <- df %>% slice_max(Sharpe, n = 1, with_ties = FALSE) %>% pull(w_matrix) %>% .[[1]]
    update_all_sliders(best_w)
  })
  
  # 當點擊套用「最小波動」
  observeEvent(input$apply_minvol_opt, {
    df <- frontier_df()
    req(df)
    minvol_w <- df %>% slice_min(Volatility, n = 1, with_ties = FALSE) %>% pull(w_matrix) %>% .[[1]]
    update_all_sliders(minvol_w)
  })
  
  
  output$frontierPlot <- renderPlot({
    df <- frontier_df()
    req(df)
    
    best_sharpe <- df %>% slice_max(Sharpe, n = 1, with_ties = FALSE)
    min_vol <- df %>% slice_min(Volatility, n = 1, with_ties = FALSE)
    
    # 找尋最接近使用者目前權重組合的模擬點
    user_w <- current_weights()
    dist_to_user <- sapply(df$w_matrix, function(w) sum((w - user_w)^2))
    user_pt <- df[which.min(dist_to_user), ]
    
    ggplot(df, aes(x = Volatility, y = Return)) +
      geom_point(color = "#bdc3c7", alpha = 0.3, size = 1) + # 萬點雲圖
      # 畫出 最大夏普點
      geom_point(data = best_sharpe, color = "#e74c3c", size = 6, shape = 18) +
      annotate("text", x = best_sharpe$Volatility, y = best_sharpe$Return, 
               label = "Max Sharpe", vjust = -1, color = "#e74c3c", fontface = "bold", size = 6) + # 放大點位標示文字
      # 畫出 最低波動點 (Min Variance)
      geom_point(data = min_vol, color = "#2980b9", size = 5, shape = 15) +
      annotate("text", x = min_vol$Volatility, y = min_vol$Return, 
               label = "Min Vol", hjust = -0.2, color = "#2980b9", fontface = "bold", size = 6) +
      # 畫出 目前使用者所在權重
      geom_point(data = user_pt, color = "#f39c12", size = 5, shape = 16) +
      annotate("text", x = user_pt$Volatility, y = user_pt$Return, 
               label = "Current", vjust = 1.8, color = "#f39c12", fontface = "bold", size = 6) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::percent) +
      theme_tq(base_size = 16) + # 放大基底字體
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14)
      ) +
      labs(x = "年化波動度 (Risk)", y = "預期年化報酬 (Expected Return)")
  })
  
  
  # 【產出 4：專業績效評估表】(精準手刻指標計算，速度最快且不會報錯)
  output$perfTable <- renderTable({
    mat_data <- ret_matrix_data()
    req(mat_data)
    
    w_vec <- current_weights()
    req(length(w_vec) == length(mat_data$syms))
    
    # 建構基底清單：使用者混合
    weights_list <- list("混合 (使用者自訂配置)" = w_vec)
    
    # 動態產生每個單一標的 100% 配置的基準
    for (i in seq_along(mat_data$syms)) {
      single_w <- rep(0, length(mat_data$syms))
      single_w[i] <- 1
      weights_list[[paste0("基準：100% ", mat_data$syms[i])]] <- single_w
    }
    
    rf <- input$rf_rate / 100
    
    # 對三個不同投資組合的權重計算績效
    res <- lapply(names(weights_list), function(nm) {
      w <- weights_list[[nm]]
      
      # 每日報酬序列向量
      port_ret <- mat_data$returns %*% matrix(w, ncol=1)
      port_ret_vec <- as.numeric(port_ret)
      
      ann_ret <- mean(port_ret_vec) * 252
      ann_vol <- sd(port_ret_vec) * sqrt(252)
      sharpe <- (ann_ret - rf) / ann_vol
      
      # 手動計算 Maximum Drawdown (業界求職必備能力展示)
      cum_ret <- cumprod(1 + port_ret_vec)
      roll_max <- cummax(c(1, cum_ret))[-1]
      drawdowns <- (roll_max - cum_ret) / roll_max
      mdd <- max(drawdowns)
      
      tibble(
        `投資組合對決` = nm,
        `年化報酬率` = sprintf("%.2f%%", ann_ret * 100),
        `年化波動度` = sprintf("%.2f%%", ann_vol * 100),
        `夏普比率 (Sharpe)` = sprintf("%.2f", sharpe),
        `最大回撤 (MDD)` = sprintf("%.2f%%", mdd * 100)
      )
    })
    
    # 計算大盤基準績效
    bench_df <- daily_bench_returns()
    if (!is.null(bench_df)) {
      bench_ret_vec <- tibble(date = mat_data$dates) %>%
        left_join(bench_df, by = "date") %>%
        pull(bench_ret) %>% replace_na(0)
        
      b_ann_ret <- mean(bench_ret_vec) * 252
      b_ann_vol <- sd(bench_ret_vec) * sqrt(252)
      b_sharpe <- (b_ann_ret - rf) / b_ann_vol
      
      b_cum_ret <- cumprod(1 + bench_ret_vec)
      b_roll_max <- cummax(c(1, b_cum_ret))[-1]
      b_mdd <- max((b_roll_max - b_cum_ret) / b_roll_max)
      
      bench_res <- tibble(
        `投資組合對決` = paste0("大盤基準 (", input$benchmark, ")"),
        `年化報酬率` = sprintf("%.2f%%", b_ann_ret * 100),
        `年化波動度` = sprintf("%.2f%%", b_ann_vol * 100),
        `夏普比率 (Sharpe)` = sprintf("%.2f", b_sharpe),
        `最大回撤 (MDD)` = sprintf("%.2f%%", b_mdd * 100)
      )
      res[[length(res) + 1]] <- bench_res
    }
    
    bind_rows(res)
  }, bordered = TRUE, striped = TRUE, hover = TRUE, align = "c", width = "100%", spacing = "s")
  
  
  # 【產出新增：資料匯出下載 Handler】
  output$dl_report <- downloadHandler(
    filename = function() {
      paste("Portfolio_Report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      mat_data <- ret_matrix_data()
      req(mat_data)
      
      w_vec <- current_weights()
      wts <- matrix(w_vec, ncol = 1)
      port_daily <- as.numeric(mat_data$returns %*% wts)
      
      df <- tibble(Date = mat_data$dates, Portfolio_Return = port_daily)
      
      bench_df <- daily_bench_returns()
      if (!is.null(bench_df)) {
        df <- df %>% 
          left_join(bench_df %>% rename(Date = date, Benchmark_Return = bench_ret), by = "Date") %>%
          replace_na(list(Benchmark_Return = 0))
      }
      
      # 為了專業度，我們可以在最上方加入權重資訊
      header_info <- paste0("# Portfolio Strategy Data Export\n",
                            "# Assets: ", paste(mat_data$syms, collapse=", "), "\n",
                            "# Weights: ", paste(round(w_vec * 100, 2), "%", collapse=", "), "\n")
      
      cat(header_info, file = file)
      write.table(df, file, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
    }
  )
  
  
  # ==========================================
  # 多因子選股掃描器邏輯 (Factor Screener Module)
  # ==========================================
  screener_data <- eventReactive(input$run_screener, {
    req(input$screener_universe)
    # 解析逗號分隔的股票代碼字串
    syms <- strsplit(input$screener_universe, ",")[[1]]
    syms <- trimws(syms)
    syms <- syms[syms != ""]
    
    validate(need(length(syms) > 0, "請輸入有效的觀察清單代碼。"))
    
    withProgress(message = '正在掃描股票池計算量化因子...', value = 0.5, {
      tryCatch({
        # 抓取過去 1 年的資料來算因子
        end_d <- Sys.Date()
        start_d <- end_d - 365
        
        df <- tq_get(syms, get = "stock.prices", from = start_d, to = end_d) %>% drop_na(close)
        validate(need(nrow(df) > 0, "掃描錯誤：查無資料，請確認輸入代碼。"))
        
        # 計算因子
        factor_results <- df %>%
          group_by(symbol) %>%
          arrange(date) %>%
          summarise(
            # 動能因子 (Momentum): 近 1 年累積報酬率
            Momentum_1Y = (last(close) / first(close)) - 1,
            
            # 日報酬波動率
            Daily_Rets = list(close / lag(close) - 1)
          ) %>%
          mutate(
            # 展開 list 並移除 NA 來計算標準差
            Vol_Ann = sapply(Daily_Rets, function(x) sd(na.omit(x)) * sqrt(252)),
            # 簡單夏普比率 (假設無風險利率 2%)
            Sharpe = (Momentum_1Y - 0.02) / Vol_Ann
          ) %>%
          select(-Daily_Rets) %>%
          arrange(desc(Sharpe)) # 預設依夏普比率排序
        
        factor_results
      }, error = function(e) {
        validate(need(FALSE, paste("量化因子掃描發生錯誤：", e$message)))
      })
    })
  })
  
  output$screenerTable <- renderTable({
    df <- screener_data()
    req(df)
    
    # 格式化數字輸出
    df %>%
      mutate(
        `股票代碼 (Symbol)` = symbol,
        `最新一年動能 (1Y Momentum)` = sprintf("%+.2f%%", Momentum_1Y * 100),
        `年化波動度 (Annualized Vol)` = sprintf("%.2f%%", Vol_Ann * 100),
        `風險調整後表現 (Sharpe Ratio)` = sprintf("%.2f", Sharpe)
      ) %>%
      select(`股票代碼 (Symbol)`, `最新一年動能 (1Y Momentum)`, `年化波動度 (Annualized Vol)`, `風險調整後表現 (Sharpe Ratio)`)
  }, bordered = TRUE, striped = TRUE, hover = TRUE, align = "c", width = "100%", spacing = "s")
  
}

shinyApp(ui, server)