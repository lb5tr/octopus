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

    $('#createChannel').click( function () {
        createChannel();
    });

    $('#listChannels').click( function () {
        listChannels();
    });

    $("#error").click ( function () {
        $("#error").fadeOut();
    });

    $("#logo").fadeIn();
    $("#loginBox").fadeIn();
    initWebSockets();
});

function initWebSockets()
{
    socket = new WebSocket("ws://127.0.0.1:7878/channel-manager");
    socket.onmessage = function (msg){
        dispatch(msg.data);
    };
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
            name : this.id
        }
    };

    socket.send(JSON.stringify(msg));
}

function hideMenu()
{
    $("#menu").css("display", "none");
}

function showGame()
{
    $("#game").css("display", "block");
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
        case "sendEvent": afterSendEvent(obj.payload); break;
        };
    }
    else
        if (obj.messageType == "state")
    {
        console.log("state!!");
        currentState = obj.payload;
    }
    else
    {

        $("#error").html("Error:" + obj.payload.errorDescription);
        $("#error").fadeIn();
        setTimeout(function () { $("#error").fadeOut();}, 1600);
        //        $("#log").html("Failed!</br>Description: " + obj.payload.errorDescription);
    }
}

function connectChannel(locator)
{
    return new WebSocket('ws://' + document.location.host + ':7878/' + locator);
}

function afterJoinChannel(channel)
{
    console.log(channel.channelLocator);
    var gameSocket = connectChannel(channel.channelLocator);
    hideMenu();
    showGame();
    startGame(gameSocket);
}

function afterLogout(p)
{
    UID = null;
    $("#loggedZone").css("display", "none");
}

function afterLogin(p)
{
    UID = p;
    $("#loginBox").fadeOut(function() {
        $("#loggedZone").fadeIn();
        $("#channels").fadeIn();
    });
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
        var li = '<li class="list-group-item channelLink" id="'+name+'" style="color:black;">'
            +name
            + '<span class="badge">'+channels[key].playersCount + '/' + channels[key].capacity + '</span>';

        if (channels[key].protected){
            li += '<span class="badge"><span class="glyphicon glyphicon-lock"></span></span>';
        }

        $("#channelsList").append(li);

    }
    $(".channelLink").click(joinChannel);
}
