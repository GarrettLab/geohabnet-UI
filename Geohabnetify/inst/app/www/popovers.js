
bslibTooltip <- function(
    id, title, placement = "bottom", trigger = "hover", options = NULL
){
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- tags$script(HTML(paste0("
    $(document).ready(function() {
      opts = $.extend(", options, ", {html: true});
      setTimeout(function() {
        $('#", id, "').tooltip('dispose').tooltip(opts);
      }, 500)
    });
  ")))
};
