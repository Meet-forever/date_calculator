data Date = Date 
    {   month :: Int, 
        day   :: Int,
        year  :: Int
    } deriving (Show, Eq)

-- Helper
-- Used in daysBetween
psyl = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366] -- preprorcessed sum of year with leap year
psy = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365] -- preprorcessed sum year


-- returns True if given date is a leap year; false otherwise
leapYear :: Date -> Bool
leapYear x
    | (year x) < 0 = False
    | (year x) `mod` 100 == 0 = (year x) `mod` 400 == 0
    | otherwise = (year x) `mod` 4 == 0


-- aug 31, july 31, feb 28, feb 29
-- Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
--  1   0   1   0   1   0   1   1   0   1   0   1
--  1   2   3   4   5   6   7   8   9   10  11  12
tomorrow x
    | (month x) == 12 && (day x) == 31 = (Date 1 1 ((year x)+1))
    | (month x) == 2 && (day x) == 28 = if (leapYear x) then (Date 2 29 (year x)) else (Date 3 1 (year x))
    | (month x) == 2 && (day x) == 29 = (Date 3 1 (year x))
    | ((month x) == 8 || (month x) == 10 || (month x) == 12) || (not ((month x) == 9 || (month x) == 11) && (month x) `mod` 2 == 1) = if (day x) == 31 then (Date ((month x)+1) 1 (year x)) else (Date (month x) ((day x)+1) (year x))  
    | ((month x) == 9 || (month x) == 11) || (not ((month x) == 8 || (month x) == 10 || (month x) == 12) && (month x) `mod` 2 == 0) = if (day x) == 30 then (Date ((month x)+1) 1 (year x)) else (Date (month x) ((day x)+1) (year x))  


-- returns date of previous day for a given date
yesterday :: Date -> Date
yesterday x 
    | (month x) == 1 && (day x) == 1 = (Date 12 31 ((year x)-1))
    | (month x) == 3 && (day x) == 1 = if (leapYear x) then (Date 2 29 (year x)) else (Date 2 28 (year x))
    | (day x) == 1 && ((month x) == 9 || (month x) == 11) =  (Date ((month x)-1) 31 (year x))
    | (day x) == 1 && ((month x) == 12 || (month x) == 10) = (Date ((month x)-1) 30 (year x))
    | (month x) `mod` 2 == 0 = if ((day x) == 1) then (Date ((month x)-1) 31 (year x)) else (Date (month x) ((day x)-1) (year x))
    | (month x) `mod` 2 == 1 = if ((day x) == 1) then (Date ((month x)-1) 30 (year x)) else (Date (month x) ((day x)-1) (year x))


-- returns date of 1st of next month for a given date
firstOfNextMonth :: Date -> Date
firstOfNextMonth x
    | (month x) == 12 = (Date 1 1 ((year x)+1))
    | otherwise = (Date ((month x)+1) 1 (year x)) 


-- returns date of 1st of previous month for a given date
firstOfPreviousMonth :: Date -> Date
firstOfPreviousMonth x
    | (month x) == 1 = (Date 12 1 ((year x)-1))
    | otherwise = (Date ((month x)-1) 1 (year x))


-- Not an Efficient approach
-- Given a positive integer, n, and a date, d, returns the
-- date for n days after d
add :: Int -> Date -> Date
add i d
    | i == 0 = d
    | otherwise = add (i-1) (tomorrow d)


-- Not an Efficient approach
-- Given a positive integer, n, and a date, d, returns the
-- date for n days before d
sub :: Int -> Date -> Date
sub i d
    | i == 0 = d
    | otherwise = sub (i-1) (yesterday d)


-- Given two input dates, d1 and d2, returns True if d1 is after d2; False otherwise
(>>>) :: Date -> Date -> Bool
(>>>) x y
    | (year x) /= (year y) = (year x) > (year y)
    | (month x) /= (month y) = (month x) > (month y)
    | (day x) /= (day y) = (day x) > (day y)
    | otherwise = False


-- Given two input dates, d1 and d2, returns True if d1 is same as d2; False otherwise
(===) :: Date -> Date -> Bool
(===) x y
    | (day x) == (day y) && (month x) == (month y) && (year x) == (year y) = True
    | otherwise = False


-- Given two input dates, d1 and d2, returns True if d1 is before d2; False otherwise
(<<<) :: Date -> Date -> Bool
(<<<) x y
    | (year x) /= (year y) = (year x) < (year y)
    | (month x) /= (month y) = (month x) < (month y)
    | (day x) /= (day y) = (day x) < (day y)
    | otherwise = False


--IDEA-----------------------------------------------------------------------------------------------------
-- sort two dates daysbetween (2016) (2020) => (2020) (2016) or daysbetween (2020) (2016) => (2020) (2016)

-- If two years are same then do casual subtraction

-- Otherwise:
-- Part 1 for month and date
-- Count bigger date from current to top such that it reach that date to 1 1 year
-- Count smaller date from current to bottom such that it reach that date to 1 1 year

-- Part 2 for year
-- sub 1 year to bigger date and add 1 to smaller date; this is to remove current smaller and bigger year
-- subtract both years with abs
-- count number of leap years and add them to years 
------------------------------------------------------------------------------------------------------------

-- Given two dates, returns number of days between the two dates
daysBetween :: Date -> Date -> Int
daysBetween d1 d2
    | (year d1) == (year d2) = abs ( (abs ((day d1) - (day d2))) - (if leapYear d1 then psyl else psy) !! (abs (((month d1)-1)-((month d2)-1))) )
    | (year d1) > (year d2) = daysBetween2 d1 d2
    | otherwise = daysBetween2 d2 d1


daysBetween2 d1 d2 = (mm + mm') + (dd - dd') + yy 
    where
        dd = (day d1)
        mm =  (if (leapYear d1) then psyl else psy) !! ((month d1)-1)
        
        dd' = (day d2)
        mm' = (if (leapYear d2) then psyl else psy) !! 12 -  (if (leapYear d2) then psyl else psy) !! ((month d2)-1)
        
        yy = abs ( (year d1) - (year d2) - 1)*365 + foldr (+) 0 [ 1 | x <- [((year d2)+1)..((year d1)-1)], leapYear (Date 1 1 x)]