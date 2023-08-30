(function () {
  "use strict";

  // Toggling of nav bar
  const reveal = document.getElementById("nav-reveal");
  const nav = document.getElementById("nav");
  reveal.addEventListener("click", () => {
    nav.classList.toggle("nav-links-visible");
  });

  // Toggling of navigation section
  const sections = document.querySelectorAll("nav h2");
  for (let i = 0; i < sections.length; i++) {
    sections[i].addEventListener("click", function () {
      this.classList.toggle("collapsed");
    });
    sections[i].addEventListener("keydown", function (e) {
      if (e.key === "Enter" || e.key === " ") {
        this.classList.toggle("collapsed");
        e.preventDefault();
        e.stopPropagation();
      }
    });
  }

  // Search
  const searchInput = document.getElementById("search-box");
  const searchResults = document.getElementById("search-results");

  document.addEventListener("keydown", e => {
    // Allow pressing Ctrl+K to focus the input box.
    if (e.key == "k" && e.ctrlKey && !e.altKey) {
      e.preventDefault();
      searchInput.focus();
    }
  });

  let loadingIndex = false;
  let index, rootPath;

  const setError = (contents) => {
    searchResults.innerHTML = "";

    const child = document.createElement("div");
    child.classList.add("search-error");
    child.innerText = contents;

    searchResults.appendChild(child);
  };

  const createElem = (tag, classes, contents) => {
    const elem = document.createElement(tag);
    if (typeof contents === "string") {
      elem.innerText = contents;
    } else if (contents instanceof Array) {
      for (const child of contents) elem.appendChild(child);
    } else {
      elem.appendChild(contents);
    }
    for (const cls of classes) elem.classList.add(cls);
    return elem;
  };

  const highlight = (match, field) => {
    const ourMatch = match.matches.find((x) => x.key === field);
    if (!ourMatch) return match.item[field] || "";

    const { value, indices } = ourMatch;

    const out = [];
    let lastIdx = 0;
    for (const [start, end] of indices) {
      if (lastIdx < start) out.push(document.createTextNode(value.substring(lastIdx, start)));
      out.push(createElem("span", ["search-match"], value.substring(start, end + 1)));
      lastIdx = end + 1;
    }
    if (lastIdx < value.length) {
      out.push(document.createTextNode(value.substring(lastIdx, value.length)));
    }

    return out;
  };

  const doSearch = () => {
    if (!index) return;

    const results = index.search(searchInput.value, { limit: 10 });
    if (results.length > 0) {
      searchResults.scrollTo(0, 0);
      searchResults.innerHTML = "";

      const list = document.createElement("ul");
      for (const result of results) {
        const link = document.createElement("a");
        link.classList.add("search-result");
        link.href = `${rootPath}${result.item.url}`;

        link.appendChild(createElem("h3", [], highlight(result, "name")));
        link.appendChild(createElem("span", ["search-page"], `${result.item.url}`));
        link.appendChild(createElem("p", ["search-summary"], highlight(result, "summary")));

        const elem = document.createElement("li");
        elem.appendChild(link);
        list.appendChild(elem);
      }

      searchResults.appendChild(list);
    } else if (searchInput.value.length === 0) {
      searchResults.innerHTML = "";
    } else {
      setError("No results found");
    }
  };

  const closeSearch = () => {
    document.body.classList.remove("searching");
    document.body.removeEventListener("click", onClick);
    searchInput.blur();
  }

  const onClick = (e) => {
    if (e.target !== searchInput) closeSearch();
  }

  searchInput.addEventListener("focus", () => {
    if (!document.body.classList.contains("searching")) {
      document.body.classList.add("searching");
      document.body.addEventListener("click", onClick);
    }

    if (!loadingIndex) {
      const indexTag = document.querySelector("meta[name=\"illuaminate:index\"]");
      const indexPath = indexTag === null ? "index.json" : indexTag.content;
      // Strip out the index.json from the path, so we've got a path to the
      // doc root.
      rootPath = indexPath.replace(/[^\/]+$/, "")

      loadingIndex = true;
      fetch(indexPath)
        .then((r) => r.json())
        .then((json) => {
          const entries = [];
          for (const [title, doc] of Object.entries(json)) {
            doc.title = title;
            entries.push(doc);
          }

          index = new Fuse(entries, {
            minMatchCharLength: 2,
            includeMatches: true,
            keys: ["name", { name: "title", score: 0.9 }, { name: "summary", score: 0.9 }],
          });

          doSearch();
        })
        .catch((e) => {
          console.error("Failed to load search index", e);
          loadingIndex = false;
          setError("Failed to load search index");
        });
    }

    doSearch();
  });

  const removeActive = (elem) => {
    elem.classList.remove("active");
    elem.ariaSelected = false;
  };
  const addActive = (elem) => {
    elem.classList.add("active");
    elem.ariaSelected = true;
    elem.scrollIntoView({
      block: "nearest",
    });
  };

  const moveNext = () => {
    const active = searchResults.querySelector("li.active");
    if (!active) {
      const elem = searchResults.querySelector("li");
      if (elem) addActive(elem);
    } else if (active.nextSibling) {
      removeActive(active);
      addActive(active.nextSibling);
    }
  }

  const movePrevious = () => {
    const active = searchResults.querySelector("li.active");
    if (active && active.previousSibling) {
      removeActive(active);
      addActive(active.previousSibling);
    }
  }

  searchInput.addEventListener("keydown", (e) => {
    switch (e.key) {
      case "Tab":
        e.preventDefault();
        if (e.shiftKey) movePrevious(); else moveNext();
        break;

      case "Down":
      case "ArrowDown":
        e.preventDefault();
        moveNext();
        break;

      case "Up":
      case "ArrowUp":
        e.preventDefault();
        movePrevious();
        break;

      case "Enter": {
        e.preventDefault();

        const link =
          searchResults.querySelector("li.active > a") || searchResults.querySelector("li > a");
        if (link) link.click();
      }

      case "Escape": {
        e.preventDefault();
        closeSearch();
        break;
      }
    }
  });

  searchInput.addEventListener("input", () => doSearch());
})();
