mol default style points
mol new trajectory.xyz waitfor all
pbc set {50.0 50.0 50.0 90.0 90.0 90.0} -all
pbc box