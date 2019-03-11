function autocomplete() {
  var autocompleteSource = document.getElementById("search_input");
  var currentFocus;

  // Display of the autocompletion
  autocompleteSource.addEventListener("input", function(e) {
    var autocompleteItems = document.getElementById("search_results");
    var currentInput = this.value;

    // Close any already open search results list
    document.getElementById("search_results").innerHTML = "";

    if (!currentInput) {
      return false;
    }

    currentFocus = -1;

    // Get autocompletion results
	var s = document.createElement("script");
    s.src = "http://google.com/complete/search?q=" + currentInput + "&client=youtube&ds=yt&jsonp=displayAutocompletionResults";
    document.body.appendChild(s);
  });

  // Select autocompletion item with keyboard
  autocompleteSource.addEventListener("keydown", function(e) {
    var autocompleteItems = document.getElementById("search_results").getElementsByTagName("div");

    if (autocompleteItems.length > 0) {
      if (e.keyCode == 40) {
        // If the down arrow key is pressed
        currentFocus++;
        addActive(autocompleteItems);

        if (autocompleteItems[currentFocus].getElementsByTagName("img").length == 0) {
          autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
        }
      }
      else if (e.keyCode == 38) {
        // If the up arrow key is pressed
        currentFocus--;
        addActive(autocompleteItems);

        if (autocompleteItems[currentFocus].getElementsByTagName("img").length == 0) {
          autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
        }
      }
      else if (e.keyCode == 13) {
        // If the enter key is pressed
        e.preventDefault();

        if (currentFocus > -1) {
          // Simulate a click on the selected item
          autocompleteItems[currentFocus].click();
          currentFocus = -1;
        }
      }
      else if (e.keyCode == 27) {
        // If the escape key is pressed
        e.preventDefault();

        document.getElementById("search_results").innerHTML = "";
        currentFocus = -1;
      }
    }
  });

  // Close search results list when the document is clicked
  document.addEventListener("click", function (e) {
      document.getElementById("search_results").innerHTML = "";
      currentFocus = -1;
  });

  function addActive(x) {
    removeActive(x);

    if (currentFocus >= x.length) {
      currentFocus = 0;
    }
    else if (currentFocus < 0) {
      currentFocus = (x.length - 1);
    }

    x[currentFocus].classList.add("search_results_active");
  }

  function removeActive(x) {
    for (var i = 0; i < x.length; i++) {
      x[i].classList.remove("search_results_active");
    }
  }
}

function displayAutocompletionResults(results) {
  var autocompleteSource = document.getElementById("search_input");
  var currentInput = autocompleteSource.value;

  for (var i = 0; i < results[1].length; i++) {
    // Create a DIV element for each result item
    resultItem = document.createElement("DIV");

    // Make the matching letters bold
    resultItem.innerHTML = "<strong>" + results[1][i][0].substr(0, currentInput.length) + "</strong>";
    resultItem.innerHTML += results[1][i][0].substr(currentInput.length);

    resultItem.setAttribute("value", results[1][i][0]);

    resultItem.addEventListener("click", function(e) {
        // Set the current input to the value of the result item and launch the search on the result item
        autocompleteSource.value = this.getAttribute("value");
        ajaxGetXmlRequest('/onclick$search_button?search_input=' + autocompleteSource.value);

        document.getElementById("search_results").innerHTML = "";
    });

    // Add the item to the list
    document.getElementById("search_results").appendChild(resultItem);
  }
}
