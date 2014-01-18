var socket = null;
var channelSocket = null;
var UID = null;
var lastWas = null;
var protectedJoinAttempt = null;


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

    $('#registrationLink').click( function () {
        showRegisterBox();
    });

    $('#register').click (function () {
        register();
    });

    $('#listChannels').click( function () {
        listChannels();
    });

    $("#error").click ( function () {
        $("#error").fadeOut();
    });

    $("#joinProtectedChannel").click ( function () {
        joinProtectedChannel();
    });

    setInterval(listChannels, 1000);

    $("#logo").fadeIn();
    $("#loginBox").fadeIn();
    initWebSockets();
});

function register()
{
    lastWas = "register";
    name = $("#newUserName").val();
    hash = $("#newUserPassword").val();

    msg = {
        messageType : "register",
        uid : "null",
        payload : {
            username : name,
            passwordHash : hash
        }
    };
    socket.send(JSON.stringify(msg));
}


function showRegisterBox()
{
    $('#loginBox').fadeOut(function () {$("#registrationBox").fadeIn();})
}

function initWebSockets()
{
    socket = new WebSocket("ws://octopus.lan:7878/channel-manager");
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

function joinProtectedChannel()
{
    lastWas = "joinChannel";
    msg = {
        messageType: "join",
        uid : UID,
        payload : {
            name : protectedJoinAttempt,
            passwordHash: CryptoJS.SHA1($("#protectedChannelPassword").val()) + ''
        }
    };

    socket.send(JSON.stringify(msg));
}

function joinChannel()
{
    if ($(this).html().indexOf('span class="glyphicon glyphicon-lock"></span></span>') > -1)
    {
        //password
        protectedJoinAttempt = this.id;
        $("#dialog").slideDown();
        return;
    }

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
        case "register": afterRegister(obj.payload); break;
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

function afterRegister(payload)
{
    $("#userName").val($("#newUserName").val());
    $("#registrationBox").fadeOut(function () {$("#loginBox").fadeIn()});
}

function connectChannel(locator)
{
    return new WebSocket('ws://' + document.location.host + ':7878/' + locator);
}

function afterJoinChannel(channel)
{
    if (protectedJoinAttempt != null)
    {
        protectedJoinAttempt = null;
        $("#dialog").slideUp();
    }
    console.log(channel.channelLocator);
    var gameSocket = connectChannel(channel.channelLocator);
    $("#channelName").html('Channel: ' + channel.name);
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
    $("#channels").fadeOut(function(){
        $("#loggedZone").fadeOut(function(){
            window.location.reload();
        });
    });
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

        $("#channelsList").append(li+'</li>');

    }
    $(".channelLink").click(joinChannel);
}
