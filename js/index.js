function index() {
  autocomplete();
}

function joinOrCreateRoom() {
  var roomName = document.getElementById("room_name_input").value;
  document.getElementById("input_form").action = "/" + roomName.toLowerCase();
  document.getElementById("room_name_input").value = roomName.toLowerCase();
}
