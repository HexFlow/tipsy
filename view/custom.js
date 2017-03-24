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
    console.log("Finding corrections for program");
    console.error("NOT YET IMPLEMENTED");
}
