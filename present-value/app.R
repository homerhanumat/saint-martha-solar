library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)

# ── Constants ───────────────────────────────────────────────────────────────
SOLAR_GROSS    <- 61250.75   # gross solar system cost
GRANT          <- 30000.00   # grant received at signing (year 0)
TAX_CREDIT     <- 19245.00   # IRS direct-pay credit received after 1 year
LOAN_AMOUNT    <- 31000.00   # loan taken out at year 0
LOAN_RATE      <- 0.04       # 4% annual interest
LOAN_YEARS     <- 10         # amortisation period
BASE_BILL      <- 2865.85    # current annual electricity bill
N_YEARS        <- 30

# Pre-compute fixed loan annual payment (standard amortising)
loan_payment <- LOAN_AMOUNT * (LOAN_RATE * (1 + LOAN_RATE)^LOAN_YEARS) /
  ((1 + LOAN_RATE)^LOAN_YEARS - 1)

# ── Helpers ─────────────────────────────────────────────────────────────────
fmt_usd <- function(x, sign = FALSE) {
  neg    <- x < 0
  prefix <- if (sign && !neg) "+$" else if (neg) "-$" else "$"
  paste0(prefix, formatC(abs(round(x)), format = "d", big.mark = ","))
}

npv_color <- function(v) if (v >= 0) "success" else "danger"

# ── Theme ────────────────────────────────────────────────────────────────────
solar_theme <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#1a7a4a",
  secondary    = "#4a90a4",
  font_scale   = 0.95
)

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = span(
    bs_icon("sun-fill", style = "color:#f4c430; margin-right:8px;"),
    "Solar PV Present Value Explorer"
  ),
  theme    = solar_theme,
  fillable = FALSE,
  
  # ── Main analysis panel ───────────────────────────────────────────────────
  nav_panel(
    "Analysis",
    icon = bs_icon("graph-up-arrow"),
    
    layout_sidebar(
      fillable = FALSE,
      
      sidebar = sidebar(
        width = 320,
        bg    = "#f8fafb",
        
        h5("Variable assumptions", class = "fw-semibold mb-3"),
        
        sliderInput(
          "discount_rate",
          tooltip(
            trigger = span("Discount rate ",
                           bs_icon("info-circle", size = "0.85em")),
            "The rate you use to convert future dollars to today's dollars.
             Use your borrowing cost or expected investment return —
             e.g. 4–5% for a savings account, 7–10% for equities."
          ),
          min = 1, max = 12, value = 5, step = 0.5, post = "%"
        ),
        
        sliderInput(
          "price_growth",
          tooltip(
            trigger = span("Electricity price growth ",
                           bs_icon("info-circle", size = "0.85em")),
            "Annual rate at which electricity prices are assumed to rise.
             The proposal uses ~4.4%. The US historical average is 2–3%."
          ),
          min = 0, max = 10, value = 4.4, step = 0.1, post = "%"
        ),
        
        sliderInput(
          "heat_pump_cost",
          tooltip(
            trigger = span("Heat-pump conversion cost ",
                           bs_icon("info-circle", size = "0.85em")),
            "Estimated cost to convert from the current gas system to a
             heat-pump system. Paid in cash at year 0."
          ),
          min = 5000, max = 15000, value = 10000,
          step = 500,
          pre = "$"
        ),
        
        hr(),
        
        h6("Fixed inputs", class = "text-muted fw-semibold mb-2"),
        tags$table(
          class = "table table-sm table-borderless mb-0",
          style = "font-size:0.82rem;",
          tags$tbody(
            tags$tr(tags$td(class="text-muted","Solar gross cost"),
                    tags$td(class="fw-semibold text-end","$61,250.75")),
            tags$tr(tags$td(class="text-muted","Grant (at signing)"),
                    tags$td(class="fw-semibold text-end","$30,000.00")),
            tags$tr(tags$td(class="text-muted","IRS tax credit (yr 1)"),
                    tags$td(class="fw-semibold text-end","$19,245.00")),
            tags$tr(tags$td(class="text-muted","Loan"),
                    tags$td(class="fw-semibold text-end","$31,000 @ 4%")),
            tags$tr(tags$td(class="text-muted","Loan term"),
                    tags$td(class="fw-semibold text-end","10 years")),
            tags$tr(tags$td(class="text-muted","Annual loan payment"),
                    tags$td(class="fw-semibold text-end",
                            paste0("$", formatC(round(loan_payment),
                                                format="d", big.mark=",")))),
            tags$tr(tags$td(class="text-muted","Base annual bill"),
                    tags$td(class="fw-semibold text-end","$2,865.85")),
            tags$tr(tags$td(class="text-muted","Post-solar bill"),
                    tags$td(class="fw-semibold text-end","$0 (102% offset)"))
          )
        ),
        
        hr(),
        
        h6("Year-0 cash flow", class = "text-muted fw-semibold mb-2"),
        tableOutput("yr0_table")
      ),
      
      # ── Main ──────────────────────────────────────────────────────────────
      div(
        
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          fill = FALSE,
          
          value_box(
            title    = "Cash out at signing",
            value    = textOutput("cash_at_signing"),
            showcase = bs_icon("cash-stack"),
            theme    = "warning",
            p("after grant & loan", style="font-size:0.78rem;opacity:0.85;margin:0;")
          ),
          
          value_box(
            title    = "PV of electricity savings",
            value    = textOutput("pv_savings"),
            showcase = bs_icon("lightning-charge-fill"),
            theme    = "success",
            p("30-yr avoided bills, discounted",
              style="font-size:0.78rem;opacity:0.85;margin:0;")
          ),
          
          value_box(
            title    = "PV of all costs",
            value    = textOutput("pv_costs"),
            showcase = bs_icon("receipt"),
            theme    = "secondary",
            p("cash + loan payments − tax credit",
              style="font-size:0.78rem;opacity:0.85;margin:0;")
          ),
          
          value_box(
            title    = "Net present value",
            value    = textOutput("npv"),
            showcase = bs_icon("graph-up"),
            theme    = value_box_theme(bg = "#1a7a4a", fg = "white"),
            p("savings minus total costs",
              style="font-size:0.78rem;opacity:0.85;margin:0;")
          )
        ),
        
        br(),
        
        navset_card_underline(
          full_screen = TRUE,
          
          nav_panel(
            "Cumulative cashflow",
            icon = bs_icon("bar-chart-steps"),
            plotOutput("cashflow_plot", height = "420px")
          ),
          
          nav_panel(
            "Annual cashflows",
            icon = bs_icon("bar-chart-fill"),
            plotOutput("annual_plot", height = "420px")
          ),
          
          nav_panel(
            "Cash flow breakdown",
            icon = bs_icon("table"),
            br(),
            p(class = "text-muted", style = "font-size:0.85rem;",
              "Year-by-year detail: electricity savings, loan payments,
               and tax credit, all in present-value terms."),
            div(style = "max-height:420px; overflow-y:auto;",
                tableOutput("cf_table"))
          ),
          
          nav_panel(
            "Sensitivity — NPV",
            icon = bs_icon("grid-3x3"),
            br(),
            p(class = "text-muted", style = "font-size:0.85rem;",
              "Net present value across discount-rate and price-growth combinations,
               at the current heat-pump cost slider value."),
            tableOutput("sensitivity_table")
          )
        )
      )
    )
  ),
  
  # ── About panel ───────────────────────────────────────────────────────────
  nav_panel(
    "About",
    icon = bs_icon("info-circle"),
    
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        card_header("How the calculation works"),
        card_body(
          h6("Cash flows included", class = "fw-semibold"),
          tags$table(
            class = "table table-sm table-bordered",
            style = "font-size:0.85rem;",
            tags$thead(tags$tr(
              tags$th("Year"), tags$th("Outflows"), tags$th("Inflows")
            )),
            tags$tbody(
              tags$tr(
                tags$td("0 (signing)"),
                tags$td("Solar gross cost + heat-pump cost"),
                tags$td("Grant ($30,000) + loan ($31,000)")
              ),
              tags$tr(
                tags$td("1"),
                tags$td("Loan payment"),
                tags$td("IRS tax credit ($19,245) + electricity saving")
              ),
              tags$tr(
                tags$td("2 – 10"),
                tags$td("Loan payment"),
                tags$td("Electricity saving each year")
              ),
              tags$tr(
                tags$td("11 – 30"),
                tags$td("—"),
                tags$td("Electricity saving each year")
              )
            )
          ),
          h6("Present value formula", class = "fw-semibold mt-3"),
          p("Each future cash flow is discounted by",
            tags$code("(1 + discount_rate)^year"),
            "and summed. The NPV is the sum of all discounted inflows
             minus all discounted outflows."),
          h6("Electricity savings", class = "fw-semibold mt-3"),
          p("The current annual bill of $2,865.85 is grown each year at
             the chosen price-growth rate. Because the system offsets 102%
             of consumption, the post-solar bill is treated as $0 and the
             entire projected bill is a saving."),
          h6("Loan", class = "fw-semibold mt-3"),
          p("Standard amortising loan: $31,000 at 4% over 10 years gives
             a fixed annual payment of $",
            strong(formatC(round(loan_payment), format="d", big.mark=",")),
            ". Only the loan payment cash flows enter the NPV — the loan
             principal itself is not a separate cost because it is already
             reflected in the payments.")
        )
      ),
      
      card(
        card_header("System & financial summary"),
        card_body(
          tags$table(
            class = "table table-sm",
            tags$tbody(
              tags$tr(tags$th("Panels"),
                      tags$td("48 × Silfab SIL-440 QD")),
              tags$tr(tags$th("Capacity"),      tags$td("21.12 kW")),
              tags$tr(tags$th("Production"),    tags$td("22,968 kWh/yr")),
              tags$tr(tags$th("Offset"),        tags$td("102%")),
              tags$tr(tags$th("Inverter"),      tags$td("SolarEdge HD Wave")),
              tags$tr(tags$th("Solar gross"),   tags$td("$61,250.75")),
              tags$tr(tags$th("Grant"),         tags$td("$30,000 at signing")),
              tags$tr(tags$th("IRS credit"),    tags$td("$19,245 after yr 1")),
              tags$tr(tags$th("Loan"),          tags$td("$31,000 @ 4%, 10 yr")),
              tags$tr(tags$th("Heat pump"),     tags$td("$5,000 – $15,000")),
              tags$tr(tags$th("Panel warranty"),tags$td("30-yr performance")),
              tags$tr(tags$th("Installer"),     tags$td("Pure Power Solar"))
            )
          )
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Core calculation ──────────────────────────────────────────────────────
  calc <- reactive({
    r  <- input$discount_rate / 100
    g  <- input$price_growth  / 100
    hp <- input$heat_pump_cost
    
    # Year-0 net cash outlay (parish pays this from reserves)
    cash_yr0 <- SOLAR_GROSS + hp - GRANT - LOAN_AMOUNT
    
    # Electricity savings: bill grows at rate g, discounted at rate r
    elec_savings_pv <- sapply(1:N_YEARS, function(yr)
      BASE_BILL * (1 + g)^yr / (1 + r)^yr
    )
    
    # Loan payments: years 1–LOAN_YEARS
    loan_pmts_pv <- sapply(1:N_YEARS, function(yr)
      if (yr <= LOAN_YEARS) loan_payment / (1 + r)^yr else 0
    )
    
    # Tax credit inflow at year 1
    tax_credit_pv <- TAX_CREDIT / (1 + r)^1
    
    # PV of total costs = cash at year 0 + PV(loan payments) − PV(tax credit)
    pv_costs <- cash_yr0 + sum(loan_pmts_pv) - tax_credit_pv
    
    # PV of savings
    pv_savings <- sum(elec_savings_pv)
    
    # NPV = PV(savings) − PV(costs)
    npv_val <- pv_savings - pv_costs
    
    # Build year-by-year undiscounted net cashflow for charting
    # (positive = net inflow to parish, negative = net outflow)
    raw_cf <- numeric(N_YEARS + 1)
    raw_cf[1] <- -cash_yr0   # year 0: net inflow to parish from outside funding
    # actually we want outflows as negative from parish perspective:
    # year 0: parish pays cash_yr0
    raw_cf[1] <- -cash_yr0
    
    for (yr in 1:N_YEARS) {
      saving <- BASE_BILL * (1 + g)^yr
      loan   <- if (yr <= LOAN_YEARS) loan_payment else 0
      credit <- if (yr == 1) TAX_CREDIT else 0
      raw_cf[yr + 1] <- saving + credit - loan
    }
    
    # Discounted cashflows
    disc_cf <- raw_cf / (1 + r)^(0:N_YEARS)
    
    # Cumulative discounted cashflow
    cum_cf <- cumsum(disc_cf)
    
    # Payback: first year cumulative >= 0
    payback <- which(cum_cf >= 0)[1] - 1
    if (length(payback) == 0 || is.na(payback)) payback <- NA
    
    list(
      cash_yr0       = cash_yr0,
      pv_savings     = pv_savings,
      pv_costs       = pv_costs,
      npv_val        = npv_val,
      payback        = payback,
      elec_savings_pv= elec_savings_pv,
      loan_pmts_pv   = loan_pmts_pv,
      tax_credit_pv  = tax_credit_pv,
      raw_cf         = raw_cf,
      disc_cf        = disc_cf,
      cum_cf         = cum_cf
    )
  })
  
  # ── Value box outputs ─────────────────────────────────────────────────────
  output$cash_at_signing <- renderText({
    fmt_usd(calc()$cash_yr0)
  })
  
  output$pv_savings <- renderText({
    fmt_usd(calc()$pv_savings)
  })
  
  output$pv_costs <- renderText({
    fmt_usd(calc()$pv_costs)
  })
  
  output$npv <- renderText({
    fmt_usd(calc()$npv_val, sign = TRUE)
  })
  
  # ── Year-0 mini table in sidebar ─────────────────────────────────────────
  output$yr0_table <- renderTable({
    hp <- input$heat_pump_cost
    data.frame(
      Item = c("Solar system", "Heat pump", "Grant (in)",
               "Loan (in)", "Net parish outlay"),
      Amount = c(
        fmt_usd(-SOLAR_GROSS),
        fmt_usd(-hp),
        fmt_usd(GRANT),
        fmt_usd(LOAN_AMOUNT),
        fmt_usd(-calc()$cash_yr0)
      )
    )
  }, striped = FALSE, hover = FALSE, bordered = FALSE,
  colnames = FALSE, align = "lr", width = "100%",
  digits = 0)
  
  # ── Cumulative cashflow plot ──────────────────────────────────────────────
  output$cashflow_plot <- renderPlot({
    d  <- calc()
    df <- data.frame(year = 0:N_YEARS, cum = d$cum_cf)
    
    payback_yr <- d$payback
    pb_label   <- if (is.na(payback_yr)) "" else
      paste0("Payback: year ", payback_yr)
    
    ggplot(df, aes(x = year, y = cum)) +
      geom_hline(yintercept = 0, colour = "#888", linewidth = 0.5,
                 linetype = "dashed") +
      geom_ribbon(data = subset(df, cum >= 0),
                  aes(ymin = 0, ymax = cum),
                  fill = "#1a7a4a", alpha = 0.15) +
      geom_ribbon(data = subset(df, cum < 0),
                  aes(ymin = cum, ymax = 0),
                  fill = "#c0392b", alpha = 0.12) +
      geom_line(colour = "#1a7a4a", linewidth = 1.3) +
      geom_point(data = subset(df, year %% 5 == 0),
                 colour = "#1a7a4a", size = 3) +
      { if (!is.na(payback_yr))
        geom_vline(xintercept = payback_yr, colour = "#f4c430",
                   linewidth = 0.9, linetype = "dotted")
        else list() } +
      { if (!is.na(payback_yr))
        annotate("text", x = payback_yr + 0.4,
                 y = max(df$cum) * 0.15,
                 label = pb_label, hjust = 0,
                 size = 3.5, colour = "#b8860b")
        else list() } +
      scale_x_continuous(breaks = seq(0, 30, 5)) +
      scale_y_continuous(
        labels = function(x) paste0("$", round(x / 1000), "k")) +
      labs(
        title    = "Cumulative discounted cashflow",
        subtitle = paste0("Discount rate: ", input$discount_rate,
                          "%   |   Price growth: ", input$price_growth,
                          "%   |   Heat pump: $",
                          formatC(input$heat_pump_cost, format="d",
                                  big.mark=",")),
        x = "Year", y = "Cumulative net value (discounted)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", colour = "#1a7a4a"),
            plot.subtitle = element_text(colour = "#555", size = 11),
            panel.grid.minor = element_blank())
  })
  
  # ── Annual cashflows plot ─────────────────────────────────────────────────
  output$annual_plot <- renderPlot({
    d  <- calc()
    
    # Stacked bar: savings (positive), loan payments (negative), tax credit (positive)
    years <- 1:N_YEARS
    df <- data.frame(
      year    = rep(years, 3),
      value   = c(
        d$elec_savings_pv,
        -d$loan_pmts_pv,
        c(d$tax_credit_pv, rep(0, N_YEARS - 1))
      ),
      type = rep(c("Electricity saving", "Loan payment", "Tax credit"), each = N_YEARS)
    )
    df$type <- factor(df$type,
                      levels = c("Electricity saving", "Tax credit", "Loan payment"))
    
    ggplot(df, aes(x = year, y = value, fill = type)) +
      geom_col(width = 0.85) +
      geom_hline(yintercept = 0, colour = "#555", linewidth = 0.4) +
      scale_fill_manual(values = c(
        "Electricity saving" = "#1a7a4a",
        "Tax credit"         = "#4a90a4",
        "Loan payment"       = "#c0392b"
      )) +
      scale_x_continuous(breaks = seq(0, 30, 5)) +
      scale_y_continuous(
        labels = function(x) paste0("$", formatC(x, format="d", big.mark=","))) +
      labs(
        title    = "Annual discounted cashflows by component",
        subtitle = "Green = electricity savings  |  Blue = tax credit  |  Red = loan payments",
        x = "Year", y = "Present value ($)", fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", colour = "#1a7a4a"),
            plot.subtitle = element_text(colour = "#555", size = 11),
            panel.grid.minor  = element_blank(),
            legend.position   = "bottom")
  })
  
  # ── Detailed cashflow table ───────────────────────────────────────────────
  output$cf_table <- renderTable({
    d <- calc()
    g <- input$price_growth / 100
    r <- input$discount_rate / 100
    
    rows <- lapply(0:N_YEARS, function(yr) {
      if (yr == 0) {
        data.frame(
          Year             = 0L,
          `Electricity saving` = "—",
          `Tax credit`         = "—",
          `Loan payment`       = "—",
          `Net (discounted)`   = fmt_usd(-calc()$cash_yr0 / (1 + r)^0, sign=TRUE),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      } else {
        saving <- BASE_BILL * (1 + g)^yr / (1 + r)^yr
        credit <- if (yr == 1) d$tax_credit_pv else 0
        loan   <- if (yr <= LOAN_YEARS) d$loan_pmts_pv[yr] else 0
        net    <- saving + credit - loan
        data.frame(
          Year             = as.integer(yr),
          `Electricity saving` = fmt_usd(saving),
          `Tax credit`         = if (yr == 1) fmt_usd(credit) else "—",
          `Loan payment`       = if (yr <= LOAN_YEARS) fmt_usd(-loan) else "—",
          `Net (discounted)`   = fmt_usd(net, sign = TRUE),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    })
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
  align = "ccccc", width = "100%", digits = 0)
  
  # ── Sensitivity table ─────────────────────────────────────────────────────
  output$sensitivity_table <- renderTable({
    d_vals <- seq(2, 10, by = 2)
    g_vals <- seq(1,  7, by = 1)
    hp     <- input$heat_pump_cost
    
    mat <- outer(g_vals, d_vals, Vectorize(function(g_pct, r_pct) {
      g  <- g_pct / 100
      r  <- r_pct / 100
      cash_yr0   <- SOLAR_GROSS + hp - GRANT - LOAN_AMOUNT
      pv_savings <- sum(BASE_BILL * (1 + g)^(1:N_YEARS) / (1 + r)^(1:N_YEARS))
      pv_loans   <- sum(sapply(1:N_YEARS,
                               function(yr) if (yr <= LOAN_YEARS)
                                 loan_payment / (1+r)^yr else 0))
      pv_credit  <- TAX_CREDIT / (1 + r)
      pv_costs   <- cash_yr0 + pv_loans - pv_credit
      round(pv_savings - pv_costs)
    }))
    
    df <- as.data.frame(mat)
    colnames(df) <- paste0(d_vals, "%")
    df[] <- lapply(df, function(col)
      sapply(col, function(v) fmt_usd(v, sign = TRUE)))
    cbind("Growth \\ Discount" = paste0(g_vals, "%"), df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
  align = "c", width = "100%")
}

shinyApp(ui = ui, server = server)