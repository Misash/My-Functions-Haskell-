
data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune


ageOn ::Planet -> Float -> Float
ageOn planet x = x / (earthY *
    case planet of 
        Mercury -> 0.2408467
        Venus  -> 0.61519726
        Earth -> 1.0
        Mars ->1.8808158
        Jupiter  -> 11.862615
        Saturn  -> 29.447498
        Uranus  -> 84.016846
        Neptune  -> 164.79132
    )
    where earthY = 31557600


--Interesting Solution without where

ageOn' :: Planet -> Double -> Double
ageOn' Earth   s = s / 31557600
ageOn' Mercury s = ageOn Earth s / 0.2408467
ageOn' Venus   s = ageOn Earth s / 0.61519726
ageOn' Mars    s = ageOn Earth s / 1.8808158
ageOn' Jupiter s = ageOn Earth s / 11.862615
ageOn' Saturn  s = ageOn Earth s / 29.447498
ageOn' Uranus  s = ageOn Earth s / 84.016846
ageOn' Neptune s = ageOn Earth s / 164.79132