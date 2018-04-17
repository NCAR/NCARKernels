class DBSCAN(object):

    def __init__(self, eps=0, min_points=2):
        self.eps = eps
        self.min_points = min_points
        self.visited = []
        self.noise = []
        self.clusters = []
        self.dp = []

    def cluster(self, data_points):
        self.visited = []
        self.dp = data_points
        c = 0
        for point in data_points:
            if point not in self.visited:
                self.visited.append(point)
                neighbours = self.region_query(point)
                if len(neighbours) < self.min_points:
                    self.noise.append(point)
                else:
                    c += 1
                    self.expand_cluster(c, neighbours)

    def expand_cluster(self, cluster_number, p_neighbours):
        cluster = ("Cluster%d" % cluster_number, [])
        self.clusters.append(cluster)
        new_points = p_neighbours
        while new_points:
            new_points = self.pool(cluster, new_points)

    def region_query(self, p):
        result = []
        for d in self.dp:
            kd = 1
            if d[0] is p[0] or d[0] is p[1]:
                kd -= 0.5
            if d[1] is p[0] or d[1] is p[1]:
                kd -= 0.5
            #distance = (((d[0] - p[0])**2 + (d[1] - p[1])**2 + (d[2] - p[2])**2)**0.5)
            distance = (((kd)**2 + (d[2] - p[2])**2)**0.5)
            #distance = abs(d[2] - p[2])
            if distance <= self.eps:
                result.append(d)
        return result

    def pool(self, cluster, p_neighbours):
        new_neighbours = []
        for n in p_neighbours:
            if n not in self.visited:
                self.visited.append(n)
                n_neighbours = self.region_query(n)
                if len(n_neighbours) >= self.min_points:
                    new_neighbours = self.unexplored(p_neighbours, n_neighbours)
            for c in self.clusters:
                if n not in c[1] and n not in cluster[1]:
                    cluster[1].append(n)
        return new_neighbours

    @staticmethod
    def unexplored(x, y):
        z = []
        for p in y:
            if p not in x:
                z.append(p)
        return z

def get_data(nsamples, neighbors):
    data = []
    for i in range(nsamples):
        for j in range(nsamples):
            dp = [i, j, 0.1]
            if (i,j) in neighbors or (j,i) in neighbors:
                dp[2] += 0.5
            if i == j:
                dp[2] = 1
            data.append(dp)
    return data

if __name__ == "__main__":
    data = get_data(10, [(1,2), (5,6)])
    dbscan = DBSCAN()
    dbscan.cluster(data)
    import pdb; pdb.set_trace()

