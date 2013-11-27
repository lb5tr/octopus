var socket = null;
var channelSocket = null;
var UID = null;
var lastWas = null;

$(document).ready(function ( ) {
    
    $('#login').click( function () {
        login($("#userName").val(), $("password").val());
    });

    $('#logout').click( function () {
        logout(UID);
    });

    $('#connect').click( function () {
        initWebSockets();
    });

    $('#createChannel').click( function () {
        createChannel();
    });

    $('#listChannels').click( function () {
        listChannels();
    });

});

function initWebSockets()
{
    socket = new WebSocket($("#address").val());
    socket.onmessage = function (msg){
          dispatch(msg.data);
        };

    $("#log").html("OK");
}

function createChannel()
{
    lastWas = "createChannel";
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
    lastWas = "login";
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

function joinChannel()
{
    lastWas = "joinChannel";
    msg = {
        messageType: "join",
        uid : UID,
        payload : {
            name : this.name
        }
    };

    socket.send(JSON.stringify(msg));
}

function afterJoinChannel(channel)
{
    console.log(channel.channelLocator);
}

function dispatch (data)
{
    obj = JSON.parse(data);

    if (obj.messageType == "ok")
    {
        $("#log").html("Success!</br>");
        switch (lastWas)
        {
            case "login": afterLogin(obj.payload);break;
            case "createChannel": afterCreate(obj.payload);break;
            case "logout": afterLogout(obj.payload); break;
            case "listChannels": afterList(obj.payload); break;
            case "joinChannel": afterJoinChannel(obj.payload); break;
        };
    }
    else
    {
        $("#log").html("Failed!</br>Description: " + obj.payload.errorDescription);
    }   
}

function afterLogout(p)
{
    UID = null;
    $("#loggedZone").css("display", "none");
}

function afterLogin(p)
{
    UID = p;
    $("#loggedZone").css("display", "block");
    listChannels();
}

function afterCreate(p)
{
    $("#log").html(JSON.stringify(p));
    listChannels();
}

function logout(uid)
{
    lastWas = "logout";
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

function listChannels()
{
    lastWas = "listChannels";

    if (UID == null){
        return;
    }

    msg = {messageType : "list", uid : UID};
    socket.send(JSON.stringify(msg));
}

function afterList(channels)
{
    $("#channelsList").html("");
    for (var key in channels)
    {
        var name = channels[key].name;
        $("#channelsList").append('<a class="channelLink" name="'+name+'">'+name+'</a></br>');
    }
    $(".channelLink").click(joinChannel);
}
