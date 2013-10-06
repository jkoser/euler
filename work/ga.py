from random import random, randrange

def print_round_stats(t, pop):
    total_fitness = 0
    #print(pop)
    for c, f in pop:
        total_fitness += f
    n = len(pop)
    q1 = pop[n // 4][1]
    q2 = pop[n // 2][1]
    q3 = pop[n * 3 // 4][1]
    print("Round", t, "average fitness:", total_fitness / len(pop))
    print("quartiles:", q1, q2, q3)
    print("best fitness:", pop[0][1])
    print("best creature:", pop[0][0])
    # print("2nd creature:", pop[1][0])
    # print("3rd creature:", pop[2][0])

def ga(generator_f, fitness_f, crossover_f, mutation_f, \
        pop_size, breed_pool_size, elite_size, elite_mutations, \
        crossover_mutation_rate, iterations, \
        verbose=True):
    t = 0
    pop = [(c, fitness_f(c)) for c in (generator_f() for i in range(pop_size))]
    pop.sort(key=lambda e: e[1], reverse=True)
    while t < iterations:
        if verbose:
            print_round_stats(t, pop)
        breed_pool = [(c, random() * f) for c, f in pop]
        breed_pool.sort(key=lambda e: e[1], reverse=True)
        new_pop = pop[0:elite_size]
        for i in range(elite_mutations):
            new_pop += [(c, fitness_f(c)) for c in \
                (mutation_f(c) for c, f in pop[0:elite_size])]
        for i in range(pop_size - elite_size * 4):
            p1 = randrange(breed_pool_size)
            p2 = randrange(breed_pool_size)
            c = crossover_f(breed_pool[p1][0], breed_pool[p2][0])
            if random() < crossover_mutation_rate:
                c = mutation_f(c)
            new_pop.append((c, fitness_f(c)))
        pop = new_pop
        pop.sort(key=lambda e: e[1], reverse=True)
        t += 1
    print_round_stats(t, pop)
    return pop[0][0]
