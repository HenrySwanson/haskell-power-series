module PowerSeries
( Series
, coeffs
, series
, diff
, int
, evalAt
, expx
, sinx
, cosx
) where

-- A series is just a list of things
newtype Series a = S { coeffs :: [a] } deriving (Eq, Show)

-- Conversion of finite lists to infinite series
-- TODO Could there be performance gains from keeping finite series (polynomials) finite?
series :: (Num a) => [a] -> Series a
series xs = S $ xs ++ repeat 0

-- Defining series for (commutative) rings
instance (Num a) => Num (Series a) where

    -- Addition is easy.
    S fs + S gs = S $ zipWith(+) fs gs

    -- Multiplication is harder.
    S (f:ft) * S (g:gt) = let head = f * g
                              tail = (S $ map (f*) gt) + (S ft * S (g:gt))
                          in S $ head : coeffs tail

    -- Clear.
    negate = S . map negate . coeffs

    -- Turns the integer into a singleton list, then converts.
    fromInteger = series . (:[]) . fromInteger

    -- These don't make sense for series.
    abs x = error "Undefined"
    signum x = error "Undefined"

-- Defining series for fields
instance (Eq a, Fractional a) => Fractional (Series a) where

    -- This is a special case of division, look there for details.
    recip (S (g:gs)) = S qs
        where qs = recip g : (map(/(-g)) . coeffs) (S gs * S qs)

    -- Let f:ft = f + xF and g:gt = g + xG. Denote the quotient by q + xQ.
    -- We know (q + xQ)(g + xG) = f + xF. Expanding:
    --         qg + x*gQ + x*qG + xx*QG = f + xF
    -- Equating the first coefficients, gq = f, so q = f/g. But what of Q?
    --
    -- Equating the rest of the series tells us that:
    --         gQ + qG + x*GQ = F
    -- You'd think we want to pull out Q, but you'd be wrong!
    --         G(q + xQ) + gQ = F
    --         Q = (F - G(q + xQ)) / g
    --
    -- As self-referential as this looks, this is exactly what we want! How so?
    -- Say we want the first term of Q. We can safely ignore xQ, because it can
    -- only contribute terms with xs in them. And since we know all the other
    -- variables, we can get the first term. Great. What about the second? The
    -- only information we're missing is the second term of xQ. But that's just
    -- the first term of Q, which we already have! Similarly, all the following
    -- terms can be computed if we know the ones before them. While this would
    -- be no fun at all in an imperative language, this is exactly what Haskell
    -- excels at.
    S (f:ft) / S (g:gt)
        -- If both leading terms are zero, we can cancel an x from both series.
        | (f == a_zero) && (g == a_zero) = S ft / S gt
        -- Else, we gotta do the division.
        | otherwise = S qs
        where qs = f/g : map(/g) (coeffs (S ft - S qs * S gt))
              a_zero = fromRational 0

    -- Sticks it into a singleton list, then converts
    fromRational = series . (:[]) . fromRational

-- Derivatives. Works over any ring.
diff :: (Num a) => Series a -> Series a
diff (S fs) = S $ zipWith(*) (tail fs) (map fromInteger [1..])

-- Integrals require division, so they must work over fields.
-- Also, the constant coefficient is undetermined, so it's given as input.
int :: (Fractional a) => Series a -> a -> Series a
int (S fs) c = S $ c : zipWith(/) fs (map fromRational [1..])

-- Evaluates the series at the given value. Since this can never halt, the
-- calculation is truncated after a given number of terms (the third variable).
evalAt :: (Num a) => Series a -> a -> Int -> a
evalAt (S (f:ft)) x n
    | n < 0 = error "Must be positive"
    | n == 0 = f
    | otherwise = f + x * (evalAt (S ft) x (n - 1))

-- Amazingly enough, these are allowable definitions. Recursion <3
sinx, cosx, expx :: (Fractional a) => Series a
expx = int expx 1
sinx = int cosx 0
cosx = - int sinx (-1)

-- TODO implement extraction of coeffients from exp. generating functions
-- TODO maybe do something about dirichlet convolution?
