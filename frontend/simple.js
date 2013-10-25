var socket = null;
var uid = null;

$(document).ready(function ( ) {
    
    $('#login').click( function () {
        login($("#userName").val(), $("password").val());
    });

    $('#logout').click( function () {
        logout(uid);
    });

    $('#connect').click( function () {
        initWebSockets();
    });

});

function initWebSockets()
{
    socket = new WebSocket($("#address").val());
    socket.onmessage = function (msg){
          dispatch(msg.data);
        };
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
        $("#log").html("Success! payload : " + obj.payload);
        uid = obj.payload;
    }
    else
    {
        $("#log").html("Failed! </br>" + obj.payload.errorDescription);
    }   
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
}
