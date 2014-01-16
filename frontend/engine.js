
gameSocket = null
currentState = null
player = null;

score_sound = null;

function sendEvent(ev)
{
    lastWas = "sendEvent";
    msg = {messageType : "event", uid: UID, payload: { eventType: ev }};
    setTimeout(socket.send(JSON.stringify(msg)), 1000/3);
}

function afterSendEvent(data)
{
}

function PlayState() {
    lastYellowScore = 0;
    lastBlueScore = 0;
    firstRun = true;


    this.setup = function() {
        jaws.on_keydown("esc",  function() { jaws.switchGameState(MenuState) })
        jaws.preventDefaultKeys(["up", "down", "left", "right", "space"])
    }

    this.update = function() {
        if(jaws.pressed("left a"))  { sendEvent("left"); }
        if(jaws.pressed("right d")) { sendEvent("right"); }
        if(jaws.pressed("up w"))    { sendEvent("up"); }
        if(jaws.pressed("down s"))  { sendEvent("down"); }
        if(jaws.pressed("space")) { sendEvent("kick"); }
    }

    this.draw = function() {
        jaws.clear()
        jaws.context.drawImage(jaws.assets.get("field.png"), 0, 0);


        if (currentState){
            if (firstRun)
            {
                lastBlueScore = currentState.scoreBlue;
                lastYellowScore = currentState.scoreYellow;
                firstRun = false;
            }

            if (lastBlueScore != currentState.scoreBlue)
            {
                score_sound.play();
                lastBlueScore++;
            }

            if (lastYellowScore != currentState.scoreYellow)
            {
                score_sound.play();
                lastYellowScore++;
            }
            $('#bluePlayers').html('');
            $('#yellowPlayers').html('');
            $('#score').html("<h3>" + lastBlueScore + ' : ' +lastYellowScore + "</h3>");
            for (var i=0;i<currentState.players.length; i++)
            {
                sprt = new jaws.Sprite({image: 'player-'+currentState.players[i][5]+'.png', x: currentState.players[i][1][1], y:currentState.players[i][1][3], anchor: "center"});

                x = currentState.players[i][3][1];
                y = currentState.players[i][3][3];
                angle = Math.acos(x) * 180/3.14;

                $('#' + currentState.players[i][5] + 'Players').append('<li class="list-group-item"  style="color:black;">'+currentState.players[i][7]+'</li>');

                //console.log(angle + ' ' + x + ' ' + y);
                if (y < 0 ){
                    sprt.rotateTo(360 - angle);
                }else
                {
                    sprt.rotateTo(angle);
                }
                //                console.log(currentState.players[i][3] + ' ' +Math.acos(currentState.players[i][3])*180/3.14)
                sprt.draw();

                //                jaws.context.drawImage(jaws.assets.get("player-blue.png"), currentState.players[i][1][1]-22, currentState.players[i][1][3]-22);
            }

            jaws.context.drawImage(jaws.assets.get("ball.png"), currentState.ballInstance.position[1]-22, currentState.ballInstance.position[3]-22);


        }
    }

    function isOutsideCanvas(item) {
        return (item.x < 0 || item.y < 0 || item.x > jaws.width || item.y > jaws.height);
    }

    function forceInsideCanvas(item) {
        if(item.x < 0)                  { item.x = 0  }
        if(item.x + item.width > jaws.width)     { item.x = jaws.width - item.width }
        if(item.y < 0)                  { item.y = 0 }
        if(item.y + item.height > jaws.height)  { item.y = jaws.height - item.height }
    }
}
/*
 *
 * MenuStatex is our lobby/welcome menu were gamer can chose start, high score and settings.
 * For this example we have only implemented start. Start switches active game state by simply:
 *   jaws.switchGameState(play)   (jaws.switchGameState(PlayState) would have worked too)
 *
 */
function MenuState() {
    var index = 0
    var items = ["Start", "Settings", "Highscore"]

    this.setup = function() {
        index = 0
        jaws.on_keydown(["down","s"],       function()  { index++; if(index >= items.length) {index=items.length-1} } )
        jaws.on_keydown(["up","w"],         function()  { index--; if(index < 0) {index=0} } )
        jaws.on_keydown(["enter","space"],  function()  { if(items[index]=="Start") {jaws.switchGameState(PlayState) } } )
    }

    this.draw = function() {
        jaws.context.clearRect(0,0,jaws.width,jaws.height)
        for(var i=0; items[i]; i++) {
            // jaws.context.translate(0.5, 0.5)
            jaws.context.font = "bold 50pt terminal";
            jaws.context.lineWidth = 10
            jaws.context.fillStyle =  (i == index) ? "Red" : "Black"
            jaws.context.strokeStyle =  "rgba(200,200,200,0.0)"
            jaws.context.fillText(items[i], 30, 100 + i * 60)
        }
    }
}

function stateUpdate(data) {
    currrentState = JSON.parse(data);
}

function startGame(socket) {
    gameSocket = socket;

    socket.onmessage = function (msg) { stateUpdate(msg.data); };
    score_sound = new Audio("score.mp3");
    score_sound.play();
    jaws.assets.add("plane.png");
    jaws.assets.add("field.png");
    jaws.assets.add("player-blue.png");
    jaws.assets.add("player-yellow.png");
    jaws.assets.add("ball.png");
    jaws.start(PlayState);
}

