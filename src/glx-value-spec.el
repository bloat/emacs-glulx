(context "Glulk 32 bit values"

         (tag 32-bit)

         (specify "Test Zero"
                  (expect (glx-32) equals glx-0)
                  (expect (glx-+ glx-0 glx-0) equals glx-0)
                  (expect (glx-+ glx-1 glx-0) equals glx-1))

         (specify "Test not zero"
                  (expect (glx-32 34 32 54 255) equals (glx-32 34 32 54 255))
                  (expect (glx-+ (glx-32 34 32 54 255) (glx-32 10 12)) equals (glx-32 44 44 54 255)))

         (specify "Test addition without carry"
                  (expect (glx-+ (glx-32 20 5 34) (glx-32 0 40 100))
                          equals (glx-32 20 45 134)))

         (specify "Test addition with carry"
                  (expect (glx-+ (glx-32 255) (glx-32 1))
                          equals (glx-32 0 1)))

         (specify "Test addition with more carries"
                  (expect (glx-+ (glx-32 255 255 167 255) (glx-32 255 255 234))
                          equals (glx-32 254 255 146)))

         (specify "Test addition with overflow"
                  (expect (glx-+ (glx-32 255 255 255 255) (glx-32 2))
                          equals (glx-32 1)))

         (specify "Test subtraction without carry"
                  (expect (glx-- glx-4 glx-0) equals glx-4)
                  (expect (glx-- glx-5 glx-3) equals glx-2)
                  (expect (glx-- (glx-32 34 56 233 45) (glx-32 12 12 12 15))
                          equals (glx-32 22 44 221 30)))

         (specify "Test subtraction with carry"
                  (expect (glx-- (glx-32 256) glx-1) equals (glx-32 255)))

         (specify "Test subtraction with more carries"
                  (expect (glx-- (glx-32 10 102 5 14) (glx-32 15 107 209)) equals (glx-32 221510395)))

         (specify "Test subtraction with overflow"
                  (expect (glx-- glx-0 glx-1) equals (glx-32 255 255 255 255)))

         (specify "Test convert to int"
                  (expect (glx-32->int glx-0) equals 0)
                  (expect (glx-32->int glx-1) equals 1)
                  (expect (glx-32->int (glx-32 0 1)) equals 256)
                  (expect (glx-32->int (glx-32 1 1)) equals 257)
                  (expect (glx-32->int (glx-32 0 0 1)) equals 65536)
                  (expect (glx-32->int (glx-32 0 0 0 1)) equals 16777216)
                  (expect (glx-32->int (glx-32 255 255 255 15)) equals 268435455))

         (specify "Test increment 32-bit"
                  (expect (glx-+1 glx-0) equals glx-1)
                  (expect (glx-+1 (glx-32 255)) equals (glx-32 0 1))
                  (expect (glx-+1 (glx-32 255 255 255 15)) equals (glx-32 0 0 0 16)))

         (specify "Test convert from int"
                  (expect (glx-32 0) equals glx-0)
                  (expect (glx-32 1) equals glx-1)
                  (expect (glx-32 255) equals (glx-32 255))
                  (expect (glx-32 256) equals (glx-32 0 1))
                  (expect (glx-32 268435455) equals (glx-32 255 255 255 15))
                  (expect (glx-32 -1) equals (glx-32 255 255 255 255))
                  (expect (glx-32 -3458987) equals (glx-- glx-0 (glx-32 3458987))))

         (specify "Test truncate"
                  (expect (glx-truncate (glx-32 1 2 3 4) 1) equals (glx-32 1))
                  (expect (glx-truncate (glx-32 1 2 3 4) 2) equals (glx-32 1 2)))

         (specify "Test large value"
                  (expect (glx-32->int (glx-32 0 0 0 #xff)) throws glx-value-error))

         (specify "Test is negative"
                  (expect (glx-neg-p (glx-32 -1)))
                  (expect (glx-neg-p (glx-32 0 0 0 128)))
                  (expect (not (glx-neg-p glx-0)))
                  (expect (not (glx-neg-p glx-1)))
                  (expect (not (glx-neg-p (glx-32 255 255 255 127))))
                  (expect (glx-neg-p (glx-+1 (glx-32 255 255 255 127)))))

         (specify "Test is positive"
                  (expect (glx-pos-p (glx-32 1)))
                  (expect (glx-pos-p (glx-32 255 255 255 127)))
                  (expect (not (glx-pos-p glx-0)))
                  (expect (not (glx-pos-p (glx-32 -1))))
                  (expect (not (glx-pos-p (glx-32 0 0 0 128))))
                  (expect (glx-pos-p (glx-- (glx-32 0 0 0 128) glx-1))))

         (specify "Test bitand"
                  (expect (glx-bitand (glx-32 98 44 230 104) (glx-32 23 0 255 33))
                          equals (glx-32 2 0 230 32)))

         (specify "Test bitor"
                  (expect (glx-bitor (glx-32 98 44 230 104) (glx-32 23 0 255 33))
                          equals (glx-32 119 44 255 105)))

         (specify "Test multiplication by one byte without carry"
                  (expect (glx-*-byte (glx-32 4 5 6 7) 6)
                          equals (glx-32 24 30 36 42)))

         (specify "Test multiplication by one byte with carry"
                  (expect (glx-*-byte (glx-32 200) 200)
                          equals (glx-32 (* 200 200))))

         (specify "Test multiplication by one byte with more carries"
                  (expect (glx-*-byte (glx-32 253 200 205 1) 10)
                          equals (glx-32 226 217 9 18)))

         (specify "Test multiplication with no carry"
                  (expect (glx-* (glx-32 4 20) (glx-32 7 8))
                          equals (glx-32 28 172 160)))

         (specify "Test multiplication with overflow"
                  (expect (glx-* (glx-32 2 3 4 5) (glx-32 7 8 9 10))
                          equals (glx-32 14 37 70 114)))

         (specify "Test negative multiplication"
                  (expect (glx-* (glx-32 -1) (glx-32 -1))
                          equals glx-1))

         (specify "Test negative by positive multiplication"
                  (expect (glx-* (glx-32 -6) (glx-32 2))
                          equals (glx-32 -12)))

         (specify "Test 32 bit to signed int"
                  (expect (glx-s32->int (glx-32 -1))
                          equals -1))

         (specify "Test large negative 32 bit to signed int"
                  (expect (glx-s32->int (glx-32 1 0 0 240))
                          equals -268435455))

         (specify "Test conversion to char"
                  (expect (glx-32->char (glx-32 97)) equals ?a)
                  (expect (glx-32->char (glx-32 97 5 6)) equals ?a))

         (specify "Test truncate"
                  (expect (glx-32-trunc (glx-32 9 8 7 6) 1) equals (glx-32 6))
                  (expect (glx-32-trunc (glx-32 9 8 7 6) 2) equals (glx-32 7 6))
                  (expect (glx-32-trunc (glx-32 9 8 7 6) 3) equals (glx-32 8 7 6)))

         (specify "Test unsigned greater than"
                  (expect (glx-32-u> (glx-32 200 200 200 200)
                                     (glx-32 5 5 5 5)))
                  (expect (glx-32-u> (glx-32 5 5 5 200)
                                     (glx-32 200 200 200 5)))
                  (expect (glx-32-u> (glx-32 200 200 200 200)
                                     (glx-32 5 200 200 200)))
                  (expect (not (glx-32-u> (glx-32 200 200 200 200)
                                          (glx-32 200 200 200 200)))))

         (specify "Test unsigned less than"
                  (expect (not (glx-32-u< (glx-32 200 200 200 200)
                                          (glx-32 5 5 5 5))))
                  (expect (not (glx-32-u< (glx-32 5 5 5 200)
                                          (glx-32 200 200 200 5))))
                  (expect (not (glx-32-u< (glx-32 200 200 200 200)
                                          (glx-32 5 200 200 200))))
                  (expect (not (glx-32-u< (glx-32 200 200 200 200)
                                          (glx-32 200 200 200 200))))))


