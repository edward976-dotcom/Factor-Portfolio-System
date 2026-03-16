# 📊 投資組合與因子選股系統 (Professional Portfolio & Factor Screener)

這是一個以 R Shiny 開發的**量化投資組合分析與多因子選股儀表板**。本系統提供互動式的 N 檔股票資產配置策略、蒙地卡羅萬組投組模擬、風險平價貢獻度分析，以及對比大盤的動態歷史回測。

## 🌟 核心功能 (Key Features)

### 1. N 檔資產最佳化與蒙地卡羅模擬 (N-Asset Portfolio Optimization)
- **無限制標的擴展**：可任意輸入台股、美股代碼 (如 `2330.TW`, `AAPL`) 建立多資產投資組合。
- ⚠️ **輸入格式規範 (重要)**：標的代碼必須完全符合 **Yahoo Finance (Yahoo 股市)** 的格式。
  - **美股**：直接輸入英文代號（如 `AAPL`, `TSLA`）。
  - **台股**：必須加上 `.TW` 結尾（如 `2330.TW`, `2449.TW`）。
- **效率前緣雲圖 (Efficient Frontier Cloud)**：底層執行一萬組 Dirichlet 隨機權重生成，精準描繪效率前緣，並自動標示**最大夏普比率 (Max Sharpe)** 與 **最低波動度 (Min Volatility)** 的最佳解。
- **一鍵派發權重**：即時套用最佳化權重，連動所有圖表與參數。

### 2. 進階風險與報酬分析 (Advanced Risk & Return Metrics)
- **基準大盤對沖 (Benchmark Comparison)**：將您的投組累積財富對比大盤基準 (如 `^TWII` 或 `^GSPC`)，評估是否成功創造超額報酬 (Alpha)。
  - ⚠️ **大盤指數格式提醒**：填寫大盤指標時，**前方務必包含 `^` 符號**，不可只填寫純英文代碼。例如：
    - ✅ 正確：`^TWII`, `^GSPC`, `^DJI`
    - ❌ 錯誤：`TWII`, `GSPC`, `DJI`
- **各標的風險貢獻度 (Risk Contribution / Marginal Risk)**：實作**風險平價 (Risk Parity)** 觀念，直觀呈現每檔資產在目前權重下對總投組的邊際風險貢獻度。
- **專業績效指標明細**：瞬間產出包含年化報酬率、年化波動度、夏普比率 (Sharpe Ratio) 及最大回撤 (MDD) 的對戰表格。
- **動態相關性熱圖 (Rolling Correlation Heatmap)**：追蹤標的間近期相關性變化，幫助有效分散風險。

### 3. 多因子選股掃描器 (Factor Screener)
- 批次輸入觀察清單，系統自動計算每檔股票的關鍵量化指標：
  - **動能因子 (1Y Momentum)**：衡量過去一年的價格強勢程度，捕捉市場趨勢。
  - **年化波動度 (Annualized Volatility)**：評估個股的價格風險與穩定性。
  - **簡單夏普比率 (Sharpe Ratio)**：綜合評估風險報酬比。 
- 幫助使用者在進入投資組合配重前，先以客觀數據篩選出具有潛力的標的。

### 4. 數據匯出 (Data Export)
- **歷史報酬與權重匯出**：一鍵將最佳化後的模型配置、每日投組歷史報酬與大盤對比數據匯出為 CSV 報表，方便後續產製研究報告。

## 🛠️ 安裝與執行 (Installation & Usage)

### 系統需求
請確保您的系統已安裝最新的 R 環境與 RStudio。

### 所需套件 (Dependencies)
本專案依賴以下 R 套件，請於執行前安裝：
```R
install.packages(c("shiny", "tidyquant", "tidyverse", "bslib", "zoo", "reshape2"))
```

### 執行方法
1. 下載本專案至本機。
2. 啟動 `Portfolio & Factor Investing System.R` (或 `app.R` 根據您的命名)。
3. 在 RStudio 中點選 `Run App`，即可開啟完整的互動式儀表板。

## 📉 注意事項與聲明 (Disclaimer& Acknowledgements)
- **免責聲明**：本專案為開源專案，提供量化技術與介面開發展示用。系統計算之回測績效與最佳化權重均基於歷史數據，**不代表未來投資收益之保證**。本工具非構成任何財務投資建議。
- **🤖 AI 協作聲明 (Co-authored with AI)**：本專案的程式碼、邏輯架構與說明文件，是由開發者與 AI 助理共同協作開發完成，並非全由個人獨立撰寫。藉此感謝 AI 技術在開發過程中提供的靈感與重構協助。

---
*Built with R Shiny, tidyquant, and bslib*
