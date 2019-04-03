function autocomplete() {
  var autocompleteSource = document.getElementById("room_name_input");
  var currentFocus;

  // Display of the autocompletion
  autocompleteSource.addEventListener("input", function(e) {
    var autocompleteItems = document.getElementById("room_name_autocomplete");
    var currentInput = this.value;

    // Close any already open search results list
    document.getElementById("room_name_autocomplete").innerHTML = "";

    if (!currentInput) {
      return false;
    }

    currentFocus = -1;

    // Get autocompletion results
    var s = document.createElement("script");
    s.src = "/get_rooms_list";
    document.body.appendChild(s);
  });

  // Select autocompletion item with keyboard
  autocompleteSource.addEventListener("keydown", function(e) {
    var autocompleteItems = document.getElementById("room_name_autocomplete").getElementsByTagName("div");

    if (autocompleteItems.length > 0) {
      if (e.keyCode == 40) {
        // If the down arrow key is pressed
        currentFocus++;
        addActive(autocompleteItems);

        autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
      }
      else if (e.keyCode == 38) {
        // If the up arrow key is pressed
        currentFocus--;
        addActive(autocompleteItems);

        autocompleteSource.value = autocompleteItems[currentFocus].getAttribute("value");
      }
      else if (e.keyCode == 13) {
        // If the enter key is pressed
        if (currentFocus > -1) {
          // Simulate a click on the selected item
          autocompleteItems[currentFocus].click();
          currentFocus = -1;
        }
      }
      else if (e.keyCode == 27) {
        // If the escape key is pressed
        e.preventDefault();

        document.getElementById("room_name_autocomplete").innerHTML = "";
        currentFocus = -1;
      }
    }
  });

  // Close search results list when the document is clicked
  document.addEventListener("click", function (e) {
      document.getElementById("room_name_autocomplete").innerHTML = "";
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

    x[currentFocus].classList.add("item_active");
  }

  function removeActive(x) {
    for (var i = 0; i < x.length; i++) {
      x[i].classList.remove("item_active");
    }
  }
}

function displayRoomNameAutocompletion(results) {
  var autocompleteSource = document.getElementById("room_name_input");
  var currentInput = autocompleteSource.value;

  for (var i = 0; i < results.length; i++) {
    // Check if the result starts with the current input
    if (results[i].substr(0, currentInput.length).toLowerCase() == currentInput.toLowerCase()) {
      // Create a DIV element for each result
      resultItem = document.createElement("DIV");

      // Make the matching letters bold
      resultItem.innerHTML = "<strong>" + results[i].substr(0, currentInput.length) + "</strong>";
      resultItem.innerHTML += results[i].substr(currentInput.length);

      resultItem.setAttribute("value", results[i]);

      resultItem.addEventListener("click", function(e) {
        // Set the current input to the value of the result item and launch the search on the result item
        autocompleteSource.value = this.getAttribute("value");
        document.getElementById("search_button").click();

        document.getElementById("room_name_autocomplete").innerHTML = "";
      });

      // Add the item to the list
      document.getElementById("room_name_autocomplete").appendChild(resultItem);
    }
  }
}
