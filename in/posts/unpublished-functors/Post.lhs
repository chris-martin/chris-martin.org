I first saw this technique used with `optparse-applicative`, though I cannot
remember where.

...

The next time I saw higher-kinded products was Oliver Charles presenting the
`rel8` library.

I think of the concept like this: We're giving up on the idea of actually
combining the codecs.

> data XYZ' a f = XYZ'
>     { x' :: Factor f a
>     , y' :: Factor f a
>     , z' :: Factor f a
>     }

> type Factor :: (Type -> Type) -> Type -> Type
> type family Factor context a where
>     Factor Identity a = a
>     Factor context a = context a

> abc' = XYZ'{ x' = 'a', y' = 'b', z' = 'c' } :: XYZ' Char Identity

> codecXYZ' = XYZ'{ x' = key 1, y' = key 2, z' = key 3 }
>   :: XYZ' Char (Codec Int Char)
