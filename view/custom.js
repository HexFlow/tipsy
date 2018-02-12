var editor = ace.edit("editor");
editor.setTheme("ace/theme/monokai");
editor.getSession().setMode("ace/mode/c_cpp");
editor.setShowPrintMargin(false);

function getQuesIDs() {
  $.get('/api/questions', function(data, status) {
    let len = data.length;
    for (let i=0; i<len; i++) {
      $('#question-dropdown').append('<a class="dropdown-item">' + data[i] + '</a>');
      $(".dropdown-item").on("click", function (e) {
        $("#dropdownMenuLink").html(e.target.innerHTML);
      });
    }
  });
}

function addToDB() {
  console.log("The current code is");
  console.log(editor.getValue());

  var prog = {userId: "sakshams", quesId: "1", code: editor.getValue()};

  $.ajax({
    url: '/api/submit',
    type: 'post',
    contentType: "application/json; charset=utf-8",
    traditional: true,
    success: function (data) {
      console.log(data);
    },
    data: JSON.stringify(prog)
  });
}

function findCorr() {
  if (!editor.getValue().trim()) {
    window.alert("Missing code");
    return;
  }

  var q = $("#dropdownMenuLink").text();
  if (q === "Select Question ID") {
    window.alert("Missing question ID")
    return;
  }

  var prog = { userId: "sakshams",
               quesId: q,
               code: editor.getValue()
             };

  console.log("Finding corrections for program");
  $.ajax({
    url: '/api/corrections',
    type: 'post',
    contentType: "application/json; charset=utf-8",
    traditional: true,
    success: function (data) {
      console.log(data);
      $('#results').html('<br>' + JSON.stringify(data, null, 4));
    },
    data: JSON.stringify(prog)
  });
}

$(document).ready(function() {
  getQuesIDs();
  $(".dropdown-item").on("click", function () {
    console.log("HEHEHEHEH");
  });
});
