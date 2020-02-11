/*
Collapse button click event handler
NOTE: Loads on initialization of App
*/
shinyjs.init = function() {
  $('#collapse_btn').click(function() {
  if ($('a span').hasClass('glyphicon-chevron-down')) {
    $('#collapse_btn').html('<span class="glyphicon glyphicon-chevron-up"></span> Show');
    $('#collapse_btn').toggleClass();
  } else {
    $('#collapse_btn').html('<span class="glyphicon glyphicon-chevron-down"></span> Hide');
    $('#collapse_btn').toggleClass();
  }
})};

