function ajaxGetRequest(url, cFunction) {
  var xhttp;
  xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      cFunction(this);
    }
 };
  xhttp.open("GET", url, true);
  xhttp.send();
}

function ajaxGetXmlRequest(url) {
  ajaxGetRequest(url, ajaxXmlAction);
}

function ajaxXmlAction(xml) {
  var xmlDoc = xml.responseXML;
  var replace_actions = xmlDoc.getElementsByTagName("replace");
  var i;
  for (i = 0; i < replace_actions.length; i++) {
    var placeholder = document.getElementById(replace_actions[i].getAttribute('id'));

    if (placeholder.tagName == "TEXTAREA" || placeholder.tagName == "INPUT") {
       placeholder.value = replace_actions[i].nodeValue;
    }
    else {
       placeholder.innerHTML = replace_actions[i].childNodes[0].nodeValue;
    }
  }
}
