contract TestConst {
    // Variabile costante: non dovrebbe cambiare mai!
    uint constant myNum = 10;

    function breakRules() public {
        // Tenta di cambiarla a 20.
        // Se la tua modifica funziona, questo deve fallire.
        myNum = 20;
    }
}