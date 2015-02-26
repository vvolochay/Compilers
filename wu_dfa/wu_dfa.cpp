#include <iostream>
using namespace std;
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wu_dfa.h"

#define LStack LinkedStack

class DFA
{
public:
    DFA();
    ~DFA();
    void GetRegExp();
    void InsertCatNode();
    void RegExpToPost();
    void GetEdgeNumber();
    void ThompsonConstruction();
    void SubsetConstruction();
private:
    char *exp;
    char *post;
    char *edge;
    int edgeNumber;
    int **DStates;
    int **Dtran;
    int *AcceptStates;
    int DStatesNumber;
    int DtranNumber;
    int NFAStatesNumber;
    int DFAStatesNumber;
    AdjacentTable *NFATable;
    TransitionTable *DFATable;
    int Precedence(char symbol);
    int CompArray(int *t1, int *t2);
    int MinimizeDFAStates(int **Dtran, int *AcceptStates, int DtranNumber, int edgeNumber);
   };

DFA::DFA()
{
    exp = new char[128];
    post = new char[128];
    edge = new char[128];
    edgeNumber = 0;
    NFAStatesNumber = 0;
    DFAStatesNumber = 0;
    DStatesNumber = 0;
    DtranNumber = 0;
    NFATable = new AdjacentTable();
}

DFA::~DFA()
{
    delete [] exp;
    delete [] post;
    delete [] edge;
    delete [] AcceptStates;
    NFATable->Clear();
    DFATable->Clear();
}

void DFA::GetRegExp()
{
    cout << "\ninput regular expression: " << endl;
    cin >> exp;
    for (int i=0; exp[i]!='\0'; i++)
    {
        if (exp[i] == '~')
        {
            cout << "\n'~'is forbidden！" << endl;
            getchar();
            exit(1);
        }
    }
    cout << "------------------------" << endl;
}

void DFA::InsertCatNode()
{
    int i = 0, j, len = strlen(exp);
    while (exp[i+1] != '\0')
    {
        if (((exp[i] != '(' && exp[i] != '.' && exp[i] != '|'&& exp[i+1] != '/')
             || exp[i] == ')' || exp[i] == '*' || exp[i] == '+' || exp[i] == '?')
            && (exp[i+1] != ')' && exp[i+1] != '.' && exp[i+1] != '|' 
            && exp[i+1] != '*' && exp[i+1] != '+' && exp[i+1] != '?' && exp[i+1] != '/'))
        {
            for (j=len; j>i+1; j--)
            {
                exp[j] = exp[j-1];
            }
            exp[i+1] = '.';
            len++;
            exp[len] = '\0';
            i++;
        }
        i++;
    }
    cout << "1.Insert cat-node\n"
    << exp << "\n------------------------" << endl;
}
int DFA::Precedence(char symbol)
{
    int priority;
    switch (symbol)
    {
        case '|': priority = 1; break;
        case '.': priority = 2; break;
        case '*': priority = 3; break;
        case '+': priority = 4; break;
        case '?': priority = 5; break;
        case '/': priority = 6; break;
        default:  priority = 0; break;
    }
    return priority;
}
void DFA::RegExpToPost()
{
    int i = 0, j = 0;
    char ch, cl;
    strcpy(post, "\0");
    LStack<char> *ls = new LStack<char>();
    ls->Clear();
    ls->Push('#');
    ch = exp[i];
    while (ch != '\0')
    {
        if (ch == '(')
        {
            ls->Push(ch);
            ch = exp[++i];
        }
        else if (ch == ')')
        {
            while (ls->GetTop() != '(')
            {
                post[j++] = ls->Pop();
            }
            ls->Pop();
            ch = exp[++i];
        }
        else if ((ch == '|') || (ch == '*') || (ch == '.') || (ch == '+') || (ch == '?') || (ch == '/') )
        {
            cl = ls->GetTop();
            while (Precedence(cl) >= Precedence(ch))
            {
                post[j++] = cl;
                ls->Pop();
                cl = ls->GetTop();
            }
            ls->Push(ch);
            ch = exp[++i];
        }
        else
        {
            post[j++] = ch;
            ch = exp[++i];
        }
    }
    ch = ls->Pop();
    while ((ch == '|') || (ch == '*') || (ch == '.') || (ch == '+') || (ch == '?') || (ch == '/') )
    {
        post[j++] = ch;
        ch = ls->Pop();
    }
    post[j] = '\0';
    ls->Clear();
    cout << "2.regexp to post\n"
    << post << "\n------------------------" << endl;
}

void DFA::GetEdgeNumber()
{
    int i = 0, j;
    edgeNumber = 0;
    while (post[i] != '\0')
    {
        if (post[i] == '.' || post[i] == '|' || post[i] == '*' || post[i] == '+' || post[i] == '?' || post[i] == '/')
        {
            i++;
            continue;
        }
        for (j=0; j<edgeNumber; j++)
        {
            if (post[i] == edge[j])
            {
                break;
            }
        }
        if (j == edgeNumber)
        {
            edge[edgeNumber] = post[i];
            edgeNumber++;
        }
        i++;
    }
    edge[edgeNumber] = '\0';
    cout << "3.char in regexp\n";
    for (i=0; i<edgeNumber; i++)
    {
        cout << edge[i] << ' ';
    }
    cout << "\nnumber: " << edgeNumber
    << "\n------------------------" << endl;
}

void DFA::ThompsonConstruction()
{
    int i, j;
    char ch;
    int s1, s2;
    LStack<int> *states = new LStack<int>();
    states->Clear();
    if (strlen(post) < 1)
    {
        cout << "No Valid Regular Expression Found!" << endl;
        getchar();
        exit(1);
    }
    NFATable->SetValue(0, 0);
    i = 1;
    j = 0;
    ch = post[j];
    while (ch != '\0')
    {
        if (ch == '.')
        {
            s2 = states->Pop();
            int temp1 = states->Pop();
            int temp2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertEdgeByValue(temp2, temp1, '~');
            states->Push(s1);
            states->Push(s2);
        }
        else if (ch == '|')
        {
            s2 = states->Pop();
            int temp1 = states->Pop();
            int temp2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(i, s1, '~');
            NFATable->InsertEdgeByValue(i, temp1, '~');
            NFATable->InsertEdgeByValue(temp2, i+1, '~');
            NFATable->InsertEdgeByValue(s2, i+1, '~');
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        else if (ch == '*')
        {
            s2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(i, i+1, '~');
            NFATable->InsertEdgeByValue(s2, s1, '~');
            NFATable->InsertEdgeByValue(i, s1, '~');
            NFATable->InsertEdgeByValue(s2, i+1, '~');
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        else if (ch == '+')
        {
            s2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(s2, s1, '~');
            NFATable->InsertEdgeByValue(i, s1, '~');
            NFATable->InsertEdgeByValue(s2, i+1, '~');
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        else if (ch == '?')
        {
            s2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(i, i+1, '~');
            NFATable->InsertEdgeByValue(i, s1, '~');
            NFATable->InsertEdgeByValue(s2, i+1, '~');
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        else if (ch == '/')
        {
            s2 = states->Pop();
            s1 = states->Pop();
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(i, i+1, '~');
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        else
        {
            NFATable->InsertVertex(i);
            NFATable->InsertVertex(i+1);
            NFATable->InsertEdgeByValue(i, i+1, ch);
            s1 = i;
            s2 = i+1;
            states->Push(s1);
            states->Push(s2);
            i += 2;
        }
        j++;
        ch = post[j];
    }
    s2 = states->Pop();
    s1 = states->Pop();
    NFATable->InsertEdgeByValue(0, s1, '~');
    if (! states->IsEmpty())
    {
        cout << "Some error in your input string!" << endl;
        getchar();
        exit(1);
    }
    NFAStatesNumber = s2 + 1;
}

int DFA::CompArray(int *t1, int *t2)
{
    int i = 0, j = 0, len1, len2;
    while (t1[i] != -1)
    {
        i++;
    }
    len1 = i;
    while (t2[j] != -1)
    {
        j++;
    }
    len2 = j;
    if (len1 != len2)
    {
        return 0;
    }
    for (i=0; i<len1; i++)
    {
        for (j=0; j<len2; j++)
        {
            if (t1[i] == t2[j])
            {
                break;
            }
        }
        if (j == len2)
        {
            return 0;
        }
    }
    return 1;
}

int DFA::MinimizeDFAStates(int **Dtran, int *AcceptStates, int DtranNumber, int edgeNumber)
{
    int h, i, j, k, l;
    for (i=0; i<DtranNumber-1; i++)
    {
        for (j=i+1; j<DtranNumber; j++)
        {
            if (AcceptStates[i] == AcceptStates[j])
            {
                for (k=0; k<edgeNumber; k++)
                {
                    if (Dtran[i][k] != Dtran[j][k])
                    {
                        break;
                    }
                }
                if (k == edgeNumber)
                {
                    for (l=j; l<DtranNumber-1; l++)
                    {
                        for (k=0; k<edgeNumber; k++)
                        {
                            Dtran[l][k] = Dtran[l+1][k];
                        }
                        AcceptStates[l] = AcceptStates[l+1];
                    }
                    for (l=0; l<DtranNumber-1; l++)
                    {
                        for (k=0; k<edgeNumber; k++)
                        {
                            if (Dtran[l][k] == j)
                            {
                                Dtran[l][k] = i;
                            }
                        }
                    }
                    for (h=j; h<DtranNumber; h++)
                    {
                        for (l=0; l<DtranNumber-1; l++)
                        {
                            for (k=0; k<edgeNumber; k++)
                            {
                                if (Dtran[l][k] == h+1)
                                {
                                    Dtran[l][k] = h;
                                }
                            }
                        }
                    }
                    DtranNumber--;
                    j--;
                }
            }
        }
    }
    return DtranNumber;
}

void DFA::SubsetConstruction()
{
    int i, j, k;
    DStatesNumber = 0;
    DtranNumber = 0;
    cout << "4.output NFA\n";
    NFATable->OutputNFA();
    cout << "------------------------" << endl;
    DStates = (int**)(new int*[NFAStatesNumber+1]);
    for (i=0; i<NFAStatesNumber+1; i++)
    {
        DStates[i] = new int[NFAStatesNumber+1];
    }
    Dtran = (int**)(new int*[NFAStatesNumber+1]);
    for (i=0; i<NFAStatesNumber+1; i++)
    {
        Dtran[i] = new int[edgeNumber+1];
    }
    for (i=0; i<NFAStatesNumber+1; i++)
    {
        for (j=0; j<edgeNumber+1; j++)
        {
            Dtran[i][j] = -1;
        }
    }
    AcceptStates = new int[NFAStatesNumber+1];
    for (i=0; i<NFAStatesNumber+1; i++)
    {
        AcceptStates[i] = 0;
    }
    int *T = new int[NFAStatesNumber+1];
    int *temp = new int[NFAStatesNumber+1];
    T[0] = 0;
    T[1] = -1;
    T = NFATable->Closure(T);
    DStates[DStatesNumber] = T;
    DStatesNumber++;
    k = 0;
    while (k < DStatesNumber)
    {
        for (i=0; edge[i]!='\0'; i++)
        {
            temp = NFATable->Closure(NFATable->Move(T, edge[i]));
            if (temp[0] != -1)
            {
                for (j=0; j<DStatesNumber; j++)
                {
                    if (CompArray(temp, DStates[j]) == 1)
                    {
                        Dtran[k][i] = j;
                        break;
                    }
                }
                if (j == DStatesNumber)
                {
                    DStates[DStatesNumber] = temp;
                    Dtran[k][i] = DStatesNumber;
                    DStatesNumber++;
                }
            }
        }
        k++;
        T = DStates[k];
    }
    DtranNumber = k;
    for (i=0; i<DStatesNumber; i++)
    {
        for (j=0; DStates[i][j]!= -1; j++)
        {
            if (DStates[i][j] == NFAStatesNumber - 1)
            {
                AcceptStates[i] = 1;
                break;
            }
        }
    }
    cout << "5.DStates\n";
    for (i=0; i<DStatesNumber; i++)
    {
        cout << i <<":  ";
        j = 0;
        while (DStates[i][j] != -1)
        {
            cout << DStates[i][j] << " ";
            j++;
        }
        cout << endl;
    }
    cout << "------------------------" << endl;
    cout << "6.Dtran\nstate";
    for (j=0; j<edgeNumber; j++)
    {
        cout << "    " << edge[j];
    }
    cout << "    AcceptStates" << endl;
    for (i=0; i<DtranNumber; i++)
    {
        if (i < 10)  cout << "   " << i << " ";
        else if (i < 100)  cout << "  " << i << " "; 
        else if (i < 1000)  cout << " " << i << " "; 
        else  cout << i << " "; 
        for (j=0; j<edgeNumber; j++) 
        { 
            if (Dtran[i][j] < 0)  cout << "     "; 
            else if (Dtran[i][j] < 10)  cout << "    " << Dtran[i][j]; 
            else if (Dtran[i][j] < 100)  cout << "   " << Dtran[i][j]; 
            else if (Dtran[i][j] < 1000)  cout << "  " << Dtran[i][j]; 
            else  cout << " " << Dtran[i][j]; 
        } 
        if (AcceptStates[i] == 1) 
        { 
            cout << "    Acc"; 
        } 
        cout << endl; 
    } 
    cout << "------------------------" << endl; 

    int DtranNumberAfterMinimization = MinimizeDFAStates(Dtran, AcceptStates, DtranNumber, edgeNumber); 
    while (DtranNumberAfterMinimization != DtranNumber) 
    { 
        DtranNumber = DtranNumberAfterMinimization; 
        DtranNumberAfterMinimization = MinimizeDFAStates(Dtran, AcceptStates, DtranNumber, edgeNumber); 
    } 

    DFATable = new TransitionTable(DtranNumber, edgeNumber); 
    for (i=0; i<DtranNumber; i++) 
    { 
        for (j=0; j<edgeNumber; j++) 
        { 
            DFATable->SetValue(i, j, Dtran[i][j]); 
        } 
    } 

    cout << "7.DFA\nstate"; 
    for (j=0; j<edgeNumber; j++) 
    { 
        cout << "    " << edge[j]; 
    } 
    cout << "    AcceptStates" << endl; 
    for (i=0; i<DtranNumber; i++) 
    { 
        if (i < 10)  cout << "   " << i << " "; 
        else if (i < 100)  cout << "  " << i << " "; 
        else if (i < 1000)  cout << " " << i << " "; 
        else  cout << i << " "; 
        for (j=0; j<edgeNumber; j++) 
        { 
            if (DFATable->GetValue(i, j) < 0)  cout << "     "; 
            else if (DFATable->GetValue(i, j) < 10)  cout << "    " << DFATable->GetValue(i, j); 
            else if (DFATable->GetValue(i, j) < 100)  cout << "   " << DFATable->GetValue(i, j); 
            else if (DFATable->GetValue(i, j) < 1000)  cout << "  " << DFATable->GetValue(i, j); 
            else  cout << " " << DFATable->GetValue(i, j); 
        } 
        if (AcceptStates[i] == 1) 
        { 
            cout << "    Acc"; 
        } 
        cout << endl; 
    } 
    cout << "------------------------" << endl; 
    for (i=0; i<NFAStatesNumber+1; i++) 
    { 
        delete [] DStates[i]; 
        delete [] Dtran[i]; 
    } 
    delete [] DStates; 
    delete [] Dtran; 
} 

int main()
{ 
    DFA dfa; 
    dfa.GetRegExp(); 
    dfa.InsertCatNode(); 
    dfa.RegExpToPost(); 
    dfa.GetEdgeNumber(); 
    dfa.ThompsonConstruction(); 
    dfa.SubsetConstruction(); 
    return 0;
} 