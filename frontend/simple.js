var socket = null;

$(document).ready(function ( ) {
    
    $('#login').click( function () {
        login($("#userName").val(), $("password").val());
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
        passwordHash: CryptoJS.MD5($("#password").val()) + ''
    };

    msg = {
        type : 'login',
        payload : user
    };

    socket.send(JSON.stringify(msg));
}

function dispatch (data)
{
    obj = JSON.parse(data);

    if (obj.messageType == "ok")
    {
        $("#log").html("Success! your id is " + obj.payload);
    }
    else
    {
        $("#log").html("Failed! </br>" + obj.payload.errorDescription);
    }   
}
