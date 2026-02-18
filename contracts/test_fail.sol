contract C { 
    // Se chiamato deve fallire, in quanto una variabile di stato non deve poter essere "external"
    uint external x; 
    }