contract FailVis {
    // ERRORE: receive() deve essere external, qui è public
    receive() public payable {} 
}