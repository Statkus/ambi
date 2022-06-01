function index() {
  autocomplete();
}

function joinOrCreateRoom() {
  var roomName = document.getElementById("room_name_input").value.toLowerCase().replace(/[^a-z0-9 ]/g, "");
  document.getElementById("input_form").action = "/" + roomName;
  document.getElementById("room_name_input").value = roomName;
}
