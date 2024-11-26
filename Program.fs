// For more information see https://aka.ms/fsharp-console-apps
let boardWidth = 16
let boardHeight = 8

type Paddle = {
    X: int
    Y: int
    Height: int
}

let move paddle y =
    let newY = max (min (paddle.Y + y) (boardHeight - 1)) 2
    {
        Paddle.Y = newY
        X = paddle.X
        Height = paddle.Height
    }
    
let paddleIn paddle x y =
    (paddle.X.Equals(x) && paddle.Y.Equals(y)) || 
    (paddle.X.Equals(x) && paddle.Y.Equals(y - 1)) ||
    (paddle.X.Equals(x) && paddle.Y.Equals(y + 1))
    
let mutable paddleA = {
    Paddle.Height = 3
    X = 1
    Y = 2
}

let mutable paddleB = {
    Paddle.Height = 3
    X = 16
    Y = 4
}

type Ball = {
    X: int
    Y: int
    DX: int
    DY: int
}

let mutable ball = {
    Ball.X = 4
    Y = 4
    DX = 1
    DY = 1
}

let ballIn ball x y =
    (ball.X.Equals(x) && ball.Y.Equals(y))

let update ball =
    let dx =
        if paddleIn paddleA (ball.X + ball.DX) (ball.Y) then -ball.DX
        elif paddleIn paddleB (ball.X + ball.DX) (ball.Y) then -ball.DX
        else ball.DX
    let dy =
        if ball.Y + ball.DY > boardHeight then -ball.DY
        elif ball.Y + ball.DY < 1 then -ball.DY
        else ball.DY
    let x =
        if ball.X + dx > boardWidth then 4
        elif ball.X + dx < 1 then 4
        else ball.X + dx
    let y = ball.Y + dy
    {
        Ball.X = x
        Ball.Y = y
        Ball.DX = dx
        Ball.DY = dy
    }

let sleep ms =
    async {
        do! Async.Sleep(ms * 1)
    } |> Async.RunSynchronously

let mutable gameOver = false
while not gameOver do
    System.Console.Clear()
    
    if System.Console.KeyAvailable then
        let keyChar = System.Console.ReadKey(true).KeyChar
        if keyChar.Equals('q') then gameOver <- true
        if keyChar.Equals('w') then paddleA <- move paddleA 1
        if keyChar.Equals('s') then paddleA <- move paddleA -1
        if keyChar.Equals('i') then paddleB <- move paddleB 1
        if keyChar.Equals('k') then paddleB <- move paddleB -1
    
    ball <- update ball
    
    for y in boardHeight .. -1 .. 1 do
        for x in 1..boardWidth do
            if paddleIn paddleA x y then
                printf "##"
            elif paddleIn paddleB x y then
                printf "##"
            elif ballIn ball x y then
                printf "**"
            else
                printf "  "
        printf "\n"
    
    sleep 250