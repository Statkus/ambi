function xmlActions(xml) {
  var replace_actions = xml.getElementsByTagName("replace");
  var i;

  for (i = 0; i < replace_actions.length; i++) {
    var placeholder = document.getElementById(replace_actions[i].getAttribute('id'));

    if (placeholder.tagName == "TEXTAREA" || placeholder.tagName == "INPUT") {
       placeholder.value = replace_actions[i].childNodes[0].nodeValue;
    }
    else {
       placeholder.innerHTML = replace_actions[i].childNodes[0].nodeValue;
    }
  }
}
