function ajaxGetRequest(url, cFunction, reloadPage = false) {
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      cFunction(this);

      if (reloadPage == true) {
        location.reload();
      }
    }
  };

  xhttp.open("GET", url);
  xhttp.send();
}

function ajaxGetXmlRequest(url, reloadPage = false) {
  ajaxGetRequest(url, ajaxXmlActions, reloadPage);
}

function ajaxGetRequestNoCallback(url, reloadPage = false) {
  ajaxGetRequest(url, function() {}, reloadPage);
}

function ajaxXmlActions(xhttp) {
  xmlActions(xhttp.responseXML);
}
