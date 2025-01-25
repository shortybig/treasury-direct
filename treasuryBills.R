loadNamespace("tidyverse")
loadNamespace("httr")
loadNamespace("gmailr")
loadNamespace("magrittr")

`%>%` <- magrittr::`%>%`

# scrape from the website -------------------------------------------------

tbills_neat <- httr::content(httr::GET("https://www.treasurydirect.gov/TA_WS/securities/jqsearch?format=json"))

tbills_int <- t(matrix(unlist(tbills_neat$securityList), ncol = length(tbills_neat$securityList))) |> 
  as.data.frame()

names(tbills_int) <- names(tbills_neat$securityList[[1]])

tbills_final <- tbills_int |> 
  dplyr::filter(securityType == "Bill" & type == "Bill") |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ ifelse(. == "", NA, .)
    )
  ) |> 
  dplyr::select_if(function (x) {!all(is.na(x))}) |> 
  dplyr::mutate(
    termType = sapply(strsplit(securityTerm, "-"), `[`, 2),
    securityTerm = as.numeric(sapply(strsplit(securityTerm, "-"), `[`, 1))
  ) |> 
  dplyr::mutate(
    securityWeeks = dplyr::case_when(
      termType == "Day" ~ securityTerm/7,
      termType == "Month" ~ securityTerm*4,
      termType == "Year" ~ securityTerm*52,
      termType == "Week" ~ securityTerm
    )
  ) |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("Date"),
      ~ as.POSIXct(., format = "%Y-%m-%d", tz = "America/New_York"))
  ) |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::matches(c("Rate", "Percentage", "Price")),
      ~ as.numeric(.)
    )
  ) |> 
  dplyr::mutate(
    securityTerm = as.factor(securityTerm)
  )

# plots -------------------------------------------------------------------

start_date <- "2022-01-01"

tbills_final |> 
  dplyr::filter(auctionDate >= start_date) %>% 
  
  {
    
    ggplot2::ggplot(., ggplot2::aes(auctionDate, highInvestmentRate)) + 
      ggplot2::geom_line(
        ggplot2::aes(
          group = securityTerm, 
          col = securityTerm
        ),
        linewidth = 1.5,
        alpha = 0.6
      ) + 
      ggplot2::scale_color_discrete(
        "Weeks", 
        guide = ggplot2::guide_legend(
          override.aes = list(linewidth = 4, alpha = 1)
        )
      ) +
      ggplot2::scale_x_datetime(
        date_breaks = "3 months", 
        labels = function (x) format(x, "%Y-%m")
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, max(.$highInvestmentRate, na.rm = TRUE), by = 0.5)
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#cccccc"),
        panel.grid.minor.y = ggplot2::element_line(color = "#cccccc", linetype = "dashed"),
        panel.grid.major.x = ggplot2::element_line(linetype = c("solid", "dashed")),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.key = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 18),
        legend.text = ggplot2::element_text(size = 15),
        legend.key.width = ggplot2::unit(2, "cm"),
        axis.title = ggplot2::element_text(size = 20, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 18),
        axis.text.x = ggplot2::element_text(size = 18, angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(size = 22, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 20, face = "italic"),
        text = ggplot2::element_text(family = "Arial Narrow")
      ) + 
      ggplot2::labs(
        title = "Treasury Bills",
        subtitle = paste0("since ", start_date),
        y = "Return of Investment (%)",
        x = "Issue Date"
      )
    
  }

# notification ------------------------------------------------------------
gmailr::gm_auth_configure()

gmailr::gm_oauth_client()

gmailr::gm_auth(email = "jbrannock2018@gmail.com")

test_email <- 
  gmailr::gm_mime() |> 
  gmailr::gm_to("jbrannock2018@gmail.com") |> 
  gmailr::gm_from("jbrannock2018@gmail.com") |> 
  gmailr::gm_subject("test email") |> 
  gmailr::gm_text_body("just a test body")

gmailr::gm_send_message(test_email)
