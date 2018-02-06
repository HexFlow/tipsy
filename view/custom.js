var editor = ace.edit("editor");
editor.setTheme("ace/theme/monokai");
editor.getSession().setMode("ace/mode/c_cpp");
editor.setShowPrintMargin(false);

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
    console.log("The current code is");
    console.log(editor.getValue());

    if (!editor.getValue().trim()) {
        window.alert("Missing code");
        return;
    }

    var q = document.getElementById("quesID").value.trim();
    if (!q) {
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
            $('#results').text(JSON.stringify(data, null, 4)).html();
        },
        data: JSON.stringify(prog)
    });
}
