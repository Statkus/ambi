function autocomplete() {
  var autocompleteSource = document.getElementById("search_input");
  var currentFocus;

  // Display of the autocompletion
  autocompleteSource.addEventListener("input", function(e) {
    var autocompleteItems;
    var currentInput = this.value;

    // Close any already open lists of autocompleted values
    closeAutocompletionResults();

    if (!currentInput) {
      return false;
    }

    currentFocus = -1;

    // Create a DIV element that will contain the items (autocompleted values)
    autocompleteItems = document.createElement("DIV");
    autocompleteItems.setAttribute("id", "autocomplete-list");
    autocompleteItems.setAttribute("class", "autocomplete-items");

    // Append the DIV element as a child of the container of the search bar
    this.parentNode.appendChild(autocompleteItems);

    // Get autocompletion results
	var s = document.createElement("script");
    s.src = "http://google.com/complete/search?q=" + currentInput + "&client=youtube&ds=yt&jsonp=displayAutocompletionResults";
    document.body.appendChild(s);
  });

  // Select autocompletion item with keyboard
  autocompleteSource.addEventListener("keydown", function(e) {
    var autocompleteItems = document.getElementById("autocomplete-list");

    if (autocompleteItems) {
      autocompleteItems = autocompleteItems.getElementsByTagName("div");

      if (e.keyCode == 40) {
        // If the down arrow key is pressed
        currentFocus++;
	    autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
        //addActive(x);
      }
      else if (e.keyCode == 38) {
        // If the up arrow key is pressed
        currentFocus--;
	    autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
        //addActive(x);
      }
      else if (e.keyCode == 13) {
        // If the enter key is pressed
        e.preventDefault();

        if (currentFocus > -1) {
          // Simulate a click on the selected item
          autocompleteItems[currentFocus].click();
        }
      }
    }
  });

  // Close autocompletion and search results lists when the document is clicked
  document.addEventListener("click", function (e) {
      closeAutocompletionResults();
      closeSearchResults();
  });

  //function addActive(x) {
  //  /*a function to classify an item as "active":*/
  //  if (!x) return false;
  //  /*start by removing the "active" class on all items:*/
  //  removeActive(x);
  //  if (currentFocus >= x.length) currentFocus = 0;
  //  if (currentFocus < 0) currentFocus = (x.length - 1);
  //  /*add class "autocomplete-active":*/
  //  x[currentFocus].classList.add("autocomplete-active");
  //}

  //function removeActive(x) {
  //  /*a function to remove the "active" class from all autocomplete items:*/
  //  for (var i = 0; i < x.length; i++) {
  //    x[i].classList.remove("autocomplete-active");
  //  }
  //}
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

        closeAutocompletionResults();
    });

    // Add the item to the list
    document.getElementById("autocomplete-list").appendChild(resultItem);
  }
}

function closeAutocompletionResults() {
  var x = document.getElementsByClassName("autocomplete-items");

  for (var i = 0; i < x.length; i++) {
    x[i].parentNode.removeChild(x[i]);
  }
}

function closeSearchResults() {
  var x = document.getElementById("search_results");

  for (var i = 0; i < x.childNodes.length; i++) {
    x.removeChild(x.childNodes[i]);
  }
}
