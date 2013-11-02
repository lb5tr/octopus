var socket = null;
var UID = null;
var lastWas = null;

$(document).ready(function ( ) {
    
    $('#login').click( function () {
        lastWas = "login";
        login($("#userName").val(), $("password").val());
    });

    $('#logout').click( function () {
        lastWas = "logout";
        logout(UID);
    });

    $('#connect').click( function () {
        lastWas = "connect";
        initWebSockets();
    });

    $('#createChannel').click( function () {
        lastWas = "createChannel";
        createChannel();
    });

});

function initWebSockets()
{
    socket = new WebSocket($("#address").val());
    socket.onmessage = function (msg){
          dispatch(msg.data);
        };
}

function createChannel()
{
    msg = {
        messageType : "create",
        uid : UID,
        payload : {
            name : $("#name").val(),
            capacity : $("#capacity").val(),
            map : $("#map").val()
        }};

        if ($("#cpassword").val() != ""){
            msg.payload.passwordHash = CryptoJS.SHA1($("#cpassword").val()) + '';
        }

        socket.send(JSON.stringify(msg));
}

function login(username, password)
{
    user = {
        username: $("#userName").val(),
        passwordHash: CryptoJS.SHA1($("#password").val()) + ''
    };

    msg = {
        messageType : 'login',
        payload : user
    };

    socket.send(JSON.stringify(msg));
}

function dispatch (data)
{
    obj = JSON.parse(data);

    if (obj.messageType == "ok")
    {
        $("#log").html("Success!</br>Payload: " + obj.payload);
        switch (lastWas)
        {
            case "login": afterLogin(obj.payload);break;
            case "createChannel": afterCreate(obj.payload);break;
            case "logout": afterlogout(obj.payload); break;
        };

        $("#createForm").css("display", "block");
    }
    else
    {
        $("#log").html("Failed!</br>Description: " + obj.payload.errorDescription);
    }   
}

function afterLogout(p)
{
    UID = null;
}

function afterLogin(p)
{
    UID = p;
}

function afterCreate(p)
{
    $("#log").html(JSON.stringify(p));
}

function logout(uid)
{
    if (uid == null){
        alert('Not loged in!');
        return;
    }

    msg = {
        messageType : 'logout',
        uid: uid
    };

    uid = null;
    socket.send(JSON.stringify(msg));
    $("#createForm").css("display", "none");
}
