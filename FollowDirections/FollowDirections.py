class Turtle(object):
    orient = ['n','e','s','w']
    def __init__(self):
        self.displacement = [0,0] #first is horiz, second is vert
        self.orientation = Turtle.orient[0]
    def __repr__(self):
        a = str(self.displacement[0])
        b = str(self.displacement[1])
        return a+","+b
    def __str__(self):
        a = self.__repr__()
        return a
    def change_dir(self, direction):
        if direction == "right":
            self.orientation = Turtle.orient[(Turtle.orient.index(self.orientation)+1)%4]
        elif direction =="left":
            self.orientation = Turtle.orient[(Turtle.orient.index(self.orientation)-1)%4]
    def move(self, steps):
        if self.orientation == Turtle.orient[0]:
            self.displacement[1] += steps
        elif self.orientation == Turtle.orient[1]:
            self.displacement[0] += steps
        elif self.orientation == Turtle.orient[2]:
            self.displacement[1] -= steps
        elif self.orientation == Turtle.orient[3]:
            self.displacement[0] -= steps
    
if __name__ == '__main__':
    walter = Turtle()
    peter = Turtle()
    with open('SampleInput.txt','r') as file:
        for line in file:
            a = line.split()[1]
            try:
                a = int(a)
            except:
                a = str(a)
            if type(a) is str:
                walter.change_dir(a)
            elif type(a) is int:
                walter.move(a)
    with open('ActualInput.txt','r') as file:
        for line in file:
            a = line.split()[1]
            try:
                a = int(a)
            except:
                a = str(a)
            if type(a) is str:
                peter.change_dir(a)
            elif type(a) is int:
                peter.move(a)
    print walter
    print peter
