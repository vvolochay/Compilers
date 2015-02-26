#include <iostream>
using namespace std;
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


template <class T>
class LinkedStack;


template <class T>
class StackNode
{
    friend class LinkedStack<T>;
private:
    T data;
    StackNode<T> *next;
    StackNode(T item = 0, StackNode<T> *p = NULL)
    {
        data = item;
        next = p;
    }
};


template <class T>
class LinkedStack
{
private:
    StackNode<T> *top;
public:
    LinkedStack();
    ~LinkedStack();
    bool IsEmpty(void) const;
    int Length(void) const;
    void Push(const T &item);
    T Pop(void);
    T GetTop(void);
    void Clear(void);
};

template <class T>
LinkedStack<T>::LinkedStack()
{
    top = NULL;
}

template <class T>
LinkedStack<T>::~LinkedStack()
{
    Clear();
}

template <class T>
bool LinkedStack<T>::IsEmpty(void) const
{
    return (! top);
}

template <class T>
int LinkedStack<T>::Length(void) const
{
    StackNode<T> *temp = new StackNode<T>();
    temp = top;
    int length = 0;
    while (temp)
    {
        temp = temp->next;
        length++;
    }
    return length;
}

template <class T>
void LinkedStack<T>::Push(const T &item)
{
    top = new StackNode<T>(item, top);
}

template <class T>
T LinkedStack<T>::Pop(void)
{
    if (! IsEmpty())
    {
        StackNode<T> *temp = top;
        top = top->next;
        T value = temp->data;
        delete temp;
        return value;
    }
    else
    {
        cout << "Stack Already Empty!" << endl;
        getchar();
        exit(1);
    }
}

template <class T>
T LinkedStack<T>::GetTop(void)
{
    if (! IsEmpty())
    {
        return top->data;
    }
    else
    {
        cout << "Stack Already Empty!" << endl;
        getchar();
        exit(1);
    }
}

template <class T>
void LinkedStack<T>::Clear(void)
{
    StackNode<T> *temp = new StackNode<T>();
    while (top)
    {
        temp = top;
        top = top->next;
        delete temp;
    }
}


class Edge
{
public:
    int number;
    int position;
    char weight;
    Edge *link;
    Edge();
    Edge(int num, int pos, char ch);
};
Edge::Edge()
{
    number = -1;
    position = -1;
    link = NULL;
}
Edge::Edge(int num, int pos, char ch)
{
    number = num;
    position = pos;
    weight = ch;
    link = NULL;
}


class Vertex
{
public:
    int number;
    Vertex *next;
    Edge *out;
    Vertex();
    Vertex(int num);
};
Vertex::Vertex()
{
    number = -1;
    next = NULL;
    out = NULL;
}
Vertex::Vertex(int num)
{
    number = num;
    next = NULL;
    out = NULL;
}


class AdjacentTable
{
private:
    Vertex *startVertex;
    int numOfVertices;
    int numOfEdges;
public:
    AdjacentTable();
    ~AdjacentTable();
    int GetValueByPos(int pos) const;
    int GetPosByValue(int value) const;
    char GetWeightByPos(int v1, int v2) const;
    char GetWeightByValue(int value1, int value2) const;
    void SetValue(int value, int pos);
    void InsertVertex(int value);
    void InsertEdgeByPos(int v1, int v2, char weight);
    void InsertEdgeByValue(int value1, int value2, char weight);
    void RemoveAllEdges(void);
    void Clear(void);
    int* Closure(int *T);
    int* Move(int *T, char ch);
    void OutputNFA(void);
};

AdjacentTable::AdjacentTable()
{
    numOfVertices = 1;
    numOfEdges = 0;
    startVertex = new Vertex();
}

AdjacentTable::~AdjacentTable()
{
    Vertex *p;
    Edge *q;
    p = startVertex;
    for (int i=0; i<numOfVertices; i++)
    {
        q = p->out;
        while (q)
        {
            p->out = q->link;
            delete q;
            q = p->out;
        }
        p = p->next;
    }
}

int AdjacentTable::GetValueByPos(int pos) const
{
    if ((pos >= 0) && (pos < numOfVertices))
    {
        Vertex *p = startVertex;
        for (int i=0; i<pos; i++)
        {
            p = p->next;
        }
        return p->number;
    }
    return -1;
}

int AdjacentTable::GetPosByValue(int value) const
{
    Vertex *p = startVertex;
    for (int i=0; i<numOfVertices; i++)
    {
        if (p->number == value)
        {
            return i;
        }
        p = p->next;
    }
    return -1;
}

char AdjacentTable::GetWeightByPos(int v1, int v2) const
{
    if ((v1 >= 0) && (v2 >= 0) && (v1 < numOfVertices) && (v2 < numOfVertices))
    {
        Vertex *p = startVertex;
        for (int i=0; i<v1; i++)
        {
            p = p->next;
        }
        Edge *q = p->out;
        while (q)
        {
            if (q->position == v2)
            {
                return (q->weight);
            }
            else
            {
                q = q->link;
            }
        }
    }
    return '#';
}

char AdjacentTable::GetWeightByValue(int value1, int value2) const
{
    return GetWeightByPos(GetPosByValue(value1), GetPosByValue(value2));
}

void AdjacentTable::SetValue(int value, int pos)
{
    if ((pos < 0) || (pos >= numOfVertices))
    {
        cout << "Illegal setting: The vertex doesn't exist!" << endl;
        getchar();
        exit(1);
    }
    Vertex *p = startVertex;
    for (int i=0; i<pos; i++)
    {
        p = p->next;
    }
    p->number = value;
}

void AdjacentTable::InsertVertex(int value)
{
    int pos = GetPosByValue(value);
    if ((pos >= 0) && (pos < numOfVertices))
    {
        cout << "Illegal insertion: The same vertex has existed!" << endl;
        getchar();
        exit(1);
    }
    Vertex *p = startVertex;
    while (p->next)
    {
        p = p->next;
    }
    Vertex *newVertex = new Vertex(value);
    p->next = newVertex;
    numOfVertices++;
}

void AdjacentTable::InsertEdgeByPos(int v1, int v2, char weight)
{
    if ((v1 < 0) || (v1 >= numOfVertices) || (v2 < 0) || (v2 >= numOfVertices))
    {
        cout << "Illegal insertion: The vertex doesn't exist!" << endl;
        getchar();
        exit(1);
    }
    Vertex *p = startVertex;
    for (int i=0; i<v1; i++)
    {
        p = p->next;
    }
    Edge *q = p->out;
    Edge *newEdge = new Edge(GetValueByPos(v2), v2, weight);
    if (! q)
    {
        p->out = newEdge;
        numOfEdges++;
        return;
    }
    while ((q->position != v2) && (q->link))
    {
        q = q->link;
    }
    if (q->position == v2)
    {
        cout << "Illegal insertion: The Edge has existed!" << endl;
        getchar();
        exit(1);
    }
    if (! q->link)
    {
        q->link = newEdge;
        numOfEdges++;
    }
}

void AdjacentTable::InsertEdgeByValue(int value1, int value2, char weight)
{
    int v1 = GetPosByValue(value1), v2 = GetPosByValue(value2);
    InsertEdgeByPos(v1, v2, weight);
}

void AdjacentTable::RemoveAllEdges(void)
{
    Vertex *p = startVertex;
    for (int i=0; i<numOfVertices; i++)
    {
        Edge *q = p->out;
        while (q)
        {
            p->out = q->link;
            delete q;
            q = p->out;
        }
        p = p->next;
    }
    numOfEdges = 0;
}

void AdjacentTable::Clear(void)
{
    RemoveAllEdges();
    Vertex *p = startVertex->next;
    while (p)
    {
        startVertex->next = p->next;
        delete p;
        p = startVertex->next;
    }
    numOfVertices = 1;
}

int* AdjacentTable::Closure(int *T)
{
    int i = 0, j, k = 0, l, len = 0;
    int *temp = new int[128];
    Vertex *p;
    Edge *q;
    while (T[len] != -1)
    {
        len++;
    }
    while (T[i] != -1)
    {
        for (l=0; l<k; l++)
        {
            if (T[i] == temp[l])
            {
                break;
            }
        }
        if (l == k)
        {
            temp[k] = T[i];
            k++;
        }
        int pos = GetPosByValue(T[i]);
        p = startVertex;
        for (j=0; j<pos; j++)
        {
            p = p->next;
        }
        q = p->out;
        while (q)
        {
            if (q->weight == '~')
            {
                for (l=0; l<k; l++)
                {
                    if (q->number == temp[l])
                    {
                        break;
                    }
                }
                if (l == k)
                {
                    temp[k] = q->number;
                    k++;
                    T[len++] = q->number;
                    T[len] = -1;
                }
            }
            q = q->link;
        }
        i++;
    }
    temp[k] = -1;
    return temp;
}

int* AdjacentTable::Move(int *T, char ch)
{
    int i = 0, j, k = 0, l;
    int *temp = new int[128];
    Vertex *p;
    Edge *q;
    while (T[i] != -1)
    {
        int pos = GetPosByValue(T[i]);
        p = startVertex;
        for (j=0; j<pos; j++)
        {
            p = p->next;
        }
        q = p->out;
        while (q)
        {
            if (q->weight == ch)
            {
                for (l=0; l<k; l++)
                {
                    if (q->number == temp[l])
                    {
                        break;
                    }
                }
                if (l == k)
                {
                    temp[k] = q->number;
                    k++;
                }
            }
            q = q->link;
        }
        i++;
    }
    temp[k] = -1;
    return temp;
}

void AdjacentTable::OutputNFA(void)
{
    Vertex *p = startVertex;
    Edge *q = new Edge();
    cout << "state  edge(weight)" << endl;
    for (int i=0; i<numOfVertices; i++)
    {
        cout << p->number;
        if (p->number < 10)	 cout << "      ";
        else if (p->number < 100)  cout << "     ";
        else if (p->number < 1000)	cout << "    ";
        else  cout << "   ";
        q = p->out;
        if (q)
        {
            while (q)
            {
                cout << q->number << "(" << q->weight << ") ";
                q = q->link;
            }
        }
        else
        {
            cout << "END";
        }
        cout << endl;
        p = p->next;
    }
}


class TransitionTable
{
public:
    TransitionTable(int rowNum, int colNum);
    ~TransitionTable();
    void SetValue(int i, int j, int value);
    int GetValue(int i, int j);
    void Clear(void);
private:
    int **matrix;
    int rowNumber;
    int colNumber;
};

TransitionTable::TransitionTable(int rowNum, int colNum)
{
    rowNumber = rowNum;
    colNumber = colNum;
    matrix = (int**)(new int*[rowNumber]);
    for (int i=0; i<rowNumber; i++)
    {
        matrix[i] = new int[colNumber];
    }
}

TransitionTable::~TransitionTable()
{
    Clear();
}

void TransitionTable::SetValue(int i, int j, int value)
{
    matrix[i][j] = value;
}
int TransitionTable::GetValue(int i, int j)
{
    return matrix[i][j];
}
void TransitionTable::Clear(void)
{
    for (int i=0; i<rowNumber; i++)
    {
        delete [] matrix[i];
    }
    delete matrix;
}