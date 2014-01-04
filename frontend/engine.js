gameSocket = null
currentState = null
player = null;

function sendEvent(ev)
{
    lastWas = "sendEvent";
    msg = {messageType : "event", uid: UID, payload: { eventType: ev }};
    setTimeout(socket.send(JSON.stringify(msg)), 1/30);
}

function afterSendEvent(data)
{
}

function PlayState() {
    this.setup = function() {
        jaws.on_keydown("esc",  function() { jaws.switchGameState(MenuState) })
        jaws.preventDefaultKeys(["up", "down", "left", "right", "space"])
    }

    this.update = function() {
        if(jaws.pressed("left a"))  { sendEvent("left"); }
        if(jaws.pressed("right d")) { sendEvent("right"); }
        if(jaws.pressed("up w"))    { sendEvent("up"); }
        if(jaws.pressed("down s"))  { sendEvent("down"); }
    }

    this.draw = function() {
        jaws.clear()
        jaws.context.drawImage(jaws.assets.get("field.png"), 0, 0);
        if (currentState){
            for (var i=0;i<currentState.players.length; i++)
            {
                jaws.context.drawImage(jaws.assets.get("bullet.png"), currentState.players[i][1]-5, currentState.players[i][3]-5);
            }
            jaws.context.drawImage(jaws.assets.get("ball.png"), currentState.ballInstance.position[1]-currentState.ballInstance.radius, currentState.ballInstance.position[3]-currentState.ballInstance.radius);
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
 * MenuState is our lobby/welcome menu were gamer can chose start, high score and settings.
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

    jaws.assets.add("plane.png");
    jaws.assets.add("field.png");
    jaws.assets.add("bullet.png");
    jaws.assets.add("ball.png");
    jaws.start(PlayState);
}

