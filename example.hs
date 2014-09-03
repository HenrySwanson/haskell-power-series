import PowerSeries

main :: IO ()
main = do {
    print . take 10 . coeffs $ series [1,2,3];
    print . take 10 . coeffs $ series [1] / series [1, -1];
    print . take 10 . coeffs . diff $ series [0,0,1];
    print . take 10 . coeffs $ int (series [0,0,1]) 3;
    print . take 10 . coeffs $ sinx;
    print $ evalAt cosx 3.1415926535 10;
    print . take 10 . coeffs $ series [0,1] / (series [1,-1,-1])
    }
