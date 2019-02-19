function ajaxGetRequest(url, cFunction, async = true) {
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      cFunction(this);
    }
  };

  xhttp.open("GET", url, async);
  xhttp.send();
}

function ajaxGetXmlRequest(url, async = true) {
  ajaxGetRequest(url, ajaxXmlActions, async);
}

function ajaxGetRequestNoCallback(url, async = true) {
  var xhttp = new XMLHttpRequest();
  xhttp.open("GET", url, async);
  xhttp.send();
}

function ajaxXmlActions(xhttp) {
  xmlActions(xhttp.responseXML);
}
