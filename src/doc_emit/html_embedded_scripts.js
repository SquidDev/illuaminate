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
    sections[i].addEventListener("keydown", function(e) {
      if(e.key === "Enter" || e.key === " ") {
        this.classList.toggle("collapsed");
        e.preventDefault();
        e.stopPropagation();
      }
    });
  }
})();
