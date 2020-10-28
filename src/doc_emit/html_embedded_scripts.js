(function() {
  "use strict";

  var reveal = document.querySelector("nav .nav-reveal");
  var nav = document.querySelector("nav");
  reveal.addEventListener("click", function(e) {
    nav.classList.toggle("nav-links-visible");
  });
})();
