#' Notifications 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
html_waiter <- function(message){
  tagList(
    div(style = "padding-bottom: 220px",
        waiter::spin_loaders(34),
        p(message, style = "font-size:22px")
    )
  )
}


html_sendEmail = function(){
  tagList(
    div(style = "padding-bottom: 220px",
        waiter::spin_loaders(34),
        p("Ενημέρωση αρμόδιου τμήματος ...", style = "font-size:18px")
    )
  )
}

show_waiter <- function(message, sleep = 0, id = NULL){
  waiter::waiter_show(id, color = "rgba(48, 45, 56, 0.43)", 
                      html = html_waiter(message)
  )
  Sys.sleep(sleep)
}

hide_waiter <- function(){
  waiter::waiter_hide()
}

show_toast <- function(type, title, message, ...){
  shinyFeedback::showToast(
    type = type,
    title = title,
    message = message,
    ...,
    .options = list(positionClass = "toast-top-center")
  )
}