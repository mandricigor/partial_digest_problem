
import Data.List(delete, sort)

-- function that finds solution to Partial Digest Problem
pdp s
	| (dX s) /= 2 * (length s) = error ("It is impossible to reconstruct the initial set from the set " ++ (show s))
	| otherwise = sort $ pdp' s (maximum s) [0] (maximum s) 

-- auxiliary function for Partial Digest Problem
pdp' [] el bag x = bag
pdp' s el [y] x = pdp' (s `remove` [el]) (next el (s `remove` [el])) [el, y] x
pdp' s el bag x
	| length (fit el s bag) == length bag = (pdp' ss (next el ss) (el:bag) x) 
	| length (fit (x - el) s bag) == length bag = pdp' sss (next el sss) ((x-el):bag) x
	| otherwise = pdp' s (next el s) bag x
		where
			ss = (s `remove` (fit el s bag))
			sss = (s `remove` (fit (x - el) s bag))


-- find the elements from dx that fit with the bag
fit element dx bag = [abs (element - x) | x <- bag, (abs (element - x)) `elem` dx] 

-- try to find the next eligible element in dx
next element dx
	| [x | x <- dx, x <= element] /= [] = maximum [x | x <- dx, x <= element]
	| otherwise = error (show dx) 

-- remove elements from the set dx
remove dx [] = dx
remove dx (e:elems) = remove (delete e dx) elems

-- function that checks if the size of the dX for partial digest is correct
dX x = y * (y + 1)
	where y = (floor (sqrt $ fromIntegral $ (2 * length x)::Double))
