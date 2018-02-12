var editor = ace.edit("editor");
editor.setTheme("ace/theme/monokai");
editor.getSession().setMode("ace/mode/c_cpp");
editor.setShowPrintMargin(false);

function getQuesIDs() {
  $.ajax({
    url: '/api/questions',
    type: 'GET',
    success: function(data) {
      let len = data.length;
      for (let i=0; i<len; i++) {
        $('#question-dropdown').append('<a class="dropdown-item">' + data[i] + '</a>');
        $(".dropdown-item").on("click", function (e) {
          if (e.target.innerHTML != "") {
            $("#dropdownMenuLink").html(e.target.innerHTML);
          }
        });
        $(".dropdown-item").keypress(function (e) {
          if (e.keyCode == 13) {
            $("#dropdownMenuLink").html(e.target.value);
          }
        });
      }
    },
    error: function(data) {
      Snackbar.show({text: 'Error fetching new questions: ' + data});
      console.error(data);
    }
  })
}

function showSol() {
  var q = $("#dropdownMenuLink").text().trim();
  if (q === "Select Question ID") {
    Snackbar.show({text: 'Missing question ID'});
    return;
  }

  $.ajax({
    url: '/api/getSimpleSolution/' + q,
    type: 'GET',
    success: function(data) {
      editor.setValue(data, 1);
    },
    error: function(data) {
      Snackbar.show({text: 'Error fetching solution: ' + data.responseText});
      console.error(data);
    }
  })
}

function addToDB() {
  var prog = {userId: "sakshams", quesId: "1", code: editor.getValue()};
  $.ajax({
    url: '/api/submit',
    type: 'post',
    contentType: "application/json; charset=utf-8",
    traditional: true,
    success: function (data) {
      Snackbar.show({text: data});
    },
    error: function(data) {
      Snackbar.show({text: 'Error submitting to DB: ' + data});
    },
    data: JSON.stringify(prog)
  });
}

function findCorr() {
  if (!editor.getValue().trim()) {
    Snackbar.show({text: 'Missing code in editor'});
    return;
  }

  var q = $("#dropdownMenuLink").text();
  if (q === "Select Question ID") {
    Snackbar.show({text: 'Missing question ID'});
    return;
  }

  var prog = { userId: "sakshams",
               quesId: q,
               code: editor.getValue()
             };

  $.ajax({
    url: '/api/corrections',
    type: 'post',
    contentType: "application/json; charset=utf-8",
    traditional: true,
    success: function (data) {
      $('#results').html('');
      if (data.length > 0) {
        for (let i=0; i<data.length; i++) {
          let dist = data[i].dist;
          $('#results').append('<br>');
          $('#results').append('Distance: ' + dist);
          $('#results').append('<br>');
          let diffs = JSON.stringify(data[i].diffs, null, 4)
          $('#results').append(diffs);
          $('#results').append('<br>');
          $('#results').append('<hr>');
        }
      } else {
        $('#results').html('<br>' + 'No corrections were found.');
      }
      Snackbar.show({text: 'Fetched corrections'});
    },
    error: function(data) {
      Snackbar.show({text: 'Error fetching corrections: ' + data});
    },
    data: JSON.stringify(prog)
  });
}

$(document).ready(function() {
  getQuesIDs();
});
