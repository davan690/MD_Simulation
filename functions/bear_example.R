#!/usr/bin/R

## EXAMPLE: Bear simulated population structure


bear1 <- create_population(10,10,10)
a1 <- iterate_population(bear1,50)
p1 <- ggplot(a1) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("C:10,Y:10,A:10")
p1

bear2 <- create_population(10,10,20)
a2 <- iterate_population(bear2,50)
p2 <- ggplot(a2) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("C:10,Y:10,A:20")
p2

bear3 <- create_population(10,10,30)
a3 <- iterate_population(bear3,50)
p3 <- ggplot(a3) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("C:10,Y:10,A:30")
p3

bear4 <- create_population(30,40,10)
a4 <- iterate_population(bear4,50)
p4 <- ggplot(a4) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("C:30,Y:40,A:10")
p4

bear5 <- create_population(100,100,100)
a5 <- iterate_population(bear5,100)
p5 <- ggplot(a5) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("C:100,Y:100,A:100")
p5

bear6 <- create_population(10,10,10)
a6 <- iterate_population(bear6,50,survival=c(2.5,0.3,0.5,0.7,0.7,0.8,0.99))
p6 <- ggplot(a6) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("10_10_10, Adult survival 9% Up")
 
bear7 <- create_population(10,10,100)
a7 <- iterate_population(bear7,50,survival=c(2.5,0.2,0.5,0.7,0.7,0.8,0.99))
p7 <- ggplot(a7) + geom_bar(aes(x=Iter,fill=Compartment)) + ggtitle("10_10_10, Adult survival 9% Up, child 10% down")
p7
