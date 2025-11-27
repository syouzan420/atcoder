import Text.Printf (printf)
main = readLn >>= (\r -> printf "%.6f %.6f\n" ((r::Double)*r*pi) (2*r*pi))
