(function() {
  "use strict";

  var reveal = document.querySelector("nav .nav-reveal");
  var nav = document.querySelector("nav");
  reveal.addEventListener("click", function(e) {
    nav.classList.toggle("nav-links-visible");
  });

  var sections = document.querySelectorAll("nav h2");
  for(var i = 0; i < sections.length; i++) {
    sections[i].addEventListener("click", function(e) {
      this.classList.toggle("collapsed");
    });
  }
})();
