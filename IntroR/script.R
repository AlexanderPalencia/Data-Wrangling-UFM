getwd()
# Strings
string <- 'this is a string'

class(string)
length(string)
nchar(string)

# Double
number <- 234

class(number)
typeof(number)
length(number)

# Integer
integer <- 2L

class(integer())

# Logical
logical <- FALSE
class(logical())

logical*1

as.logical(0)
as.logical(1)

# Vectores
num_vector <- c(1,2,3)
length(num_vector)

num_vector <- c(1,2,3,4,"a")

vec1 <- 1:00
vec2 <- sample(x = 1:10, size = 5, replace = FALSE)
vector("integer", length = 10)

class(num_vector)


# Apend to the vector
c(num_vector, 5, 6, 7, 8, 9)

log_vec <- c(F,F,T)
class(log_vec)

#convertir tipo de datos
as.numeric(num_vector)

as.character(num_vector)


#factor, no se le puede agregar nada que no este en sus nivels tiene orden
#factores ordenados

factor_1 <- c("Mon", "Tue", "Mon")

factor_1 <- factor(factor_1)


#listas
vector1 <- c(1,2,3,4,5)

list_1 <- list(vector1)

#matrix


mat <- matrix(1:10, nrow = 2, ncol = 5)
mat[3,]

#dataframes colecciones

nrow()
ncol()

#functions of base R(ufunctions)



