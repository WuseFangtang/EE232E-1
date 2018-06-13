#question 11
from numpy import genfromtxt
import numpy as np
from scipy.spatial import Delaunay
points = genfromtxt('location.csv', delimiter=',')
tri = Delaunay(points)
tri_edge_list = tri.simplices
import matplotlib.pyplot as plt

edge_list=[]
for i in tri_edge_list:
	edge_list.append([i[0]+1,i[1]+1])
	edge_list.append([i[0]+1,i[2]+1])
	edge_list.append([i[1]+1,i[2]+1])

import csv
with open('Delaunay_edgelist.csv', 'w', newline='') as csvfile:
    spamwriter = csv.writer(csvfile, delimiter=',',quotechar='|', quoting=csv.QUOTE_MINIMAL)
    for row in edge_list:
        spamwriter.writerow(row)

plt.triplot(points[:,0], points[:,1], tri_edge_list)
plt.plot(points[:,0], points[:,1], 'o')
plt.show()


