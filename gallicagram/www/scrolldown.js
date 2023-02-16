$(document).on('shiny:value', function(event) {
  // Scroll down after model update
  if (event.target.id === 'model_record') {
    window.scrollTo(0,document.body.scrollHeight);
  }
});
