-- the NFData class: sequential and parallel...
module NFData where

class NFData a where
      rnf :: a -> ()
      rnf x = x `seq` ()
instance NFData ()
instance NFData Int 
instance NFData Float
instance NFData Double
instance NFData Char
instance NFData Integer
instance (NFData a) => NFData [a] where
   rnf = rnfl
rnfl :: NFData a => [a] -> ()
rnfl []     = ()
rnfl (x:xs) = rnf x `seq` rnfl xs

instance (NFData a, NFData b) => NFData (a,b) 
    where rnf (a,b) = rnf a `seq` rnf b
instance (NFData a, NFData b, 
	  NFData c) => NFData (a,b,c) 
    where rnf (a,b,c) = rnf a `seq` rnf b `seq` rnf c
instance (NFData a, NFData b, 
	  NFData c, NFData d) => NFData (a,b,c,d) 
    where rnf (a,b,c,d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
instance (NFData a, NFData b, NFData c,  
	  NFData d, NFData e) => NFData (a,b,c,d,e) 
    where rnf (a,b,c,d,e) = rnf a `seq` rnf b `seq`  rnf c 
			    `seq` rnf d `seq` rnf e
instance (NFData a, NFData b, NFData c,  
	  NFData d, NFData e,NFData f) => NFData (a,b,c,d,e,f) 
    where rnf (a,b,c,d,e,f) = rnf a `seq` rnf b `seq`  rnf c 
			      `seq` rnf d `seq` rnf e `seq` rnf f
instance (NFData a, NFData b, NFData c,  NFData d, 
	  NFData e,NFData f,NFData g) => NFData (a,b,c,d,e,f,g) 
    where rnf (a,b,c,d,e,f,g) = rnf a `seq` rnf b `seq`  rnf c 
				`seq` rnf d `seq` rnf e 
				`seq` rnf f `seq` rnf g
instance (NFData a, NFData b, NFData c,  
	  NFData d, NFData e,NFData f,
	  NFData g,NFData h) => NFData (a,b,c,d,e,f,g,h) 
    where rnf (a,b,c,d,e,f,g,h) = rnf a `seq` rnf b `seq`  rnf c 
				  `seq` rnf d `seq` rnf e `seq` rnf f 
				  `seq` rnf g `seq` rnf h

instance NFData (a -> b) 
    where rnf f = ()

