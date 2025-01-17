//<-----INCLUDE HEADER FILE ------>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <time.h>
#include <limits.h>
#include <float.h>
#include <errno.h>

//<------STRUCTURE FOR POLYNOMIAL ------>

typedef struct poly
{
    int coeff;
    int pow;
    struct poly *next;
} a;

void print(a *t)
{
    while (t->next)
    {
        if (t->pow && t->coeff)
        {
            printf("%dx^%d + ", t->coeff, t->pow);
        }
        else if (t->coeff == 0)
            ;
        else if (t->pow == 0)
        {
            printf("%d + \n", t->coeff);
        }
        t = t->next;
    }
    if (t->pow && t->coeff)
    {
        printf("%dx^%d\n", t->coeff, t->pow);
    }
    else if (t->coeff == 0)
    {
        printf("0\n");
    }
    else if (t->pow == 0)
    {
        printf("%d\n", t->coeff);
    }
    // printf("%dx^%d\n", t->coeff, t->pow);
}

//<------CREATE A POLYNOMIAL TERM------->

a *term(int c, int p)
{
    a *q = (a *)malloc(sizeof(a));
    q->coeff = c;
    q->pow = p;
    q->next = NULL;
    return q;
}

//<------POLYNOMIAL ARRANGE IN ASCENDING ORDER FUNCTION----->

void Make(a **head, int c, int p)
{
    if (*head == NULL)
    {
        *head = term(c, p);
    }
    else
    {
        a *temp = *head;
        if (temp->pow > p)
        {
            while (temp->next && temp->next->pow > p)
            {
                temp = temp->next;
            }
            if (temp->next && temp->next->pow == p)
            {
                temp->next->coeff += c;
                return;
            }
            a *w = temp->next;
            temp->next = term(c, p);
            temp->next->next = w;
        }
        else if (temp->pow < p)
        {
            *head = term(c, p);
            (*head)->next = temp;
        }
        else
        {
            (*head)->coeff = (*head)->coeff + c;
        }
    }
}

//<------FUNCTION TO CREATE POLYNOMIAL------>

a *createPoly(int n)
{
    if (n <= 0)
    {
        printf("Invalid Input\n");
        return NULL;
    }
    a *head = NULL;
    for (int i = 0; i < n; i++)
    {
        int c, p;
        printf("Enter term %d coefficient and power(with a space) : ", i + 1);
        scanf("%d%d", &c, &p);
        Make(&head, c, p);
    }
    return head;
}

//<-----SUM FUNCTION---->

a *sum(a *w, a *q)
{
    a *new = NULL;
    a *tail = NULL;
    while (w && q)
    {
        if (w->pow == q->pow)
        {
            if (new == NULL)
            {
                new = term(w->coeff + q->coeff, w->pow);
                tail = new;
            }
            else
            {
                tail->next = term(w->coeff + q->coeff, w->pow);
                tail = tail->next;
            }
            w = w->next;
            q = q->next;
        }
        else if (w->pow > q->pow)
        {
            if (new == NULL)
            {
                new = term(w->coeff, w->pow);
                tail = new;
            }
            else
            {
                tail->next = term(w->coeff, w->pow);
                tail = tail->next;
            }
            w = w->next;
        }
        else
        {
            if (new == NULL)
            {
                new = term(q->coeff, q->pow);
                tail = new;
            }
            else
            {
                tail->next = term(q->coeff, q->pow);
                tail = tail->next;
            }
            q = q->next;
        }
    }
    if (w == NULL)
    {
        while (q)
        {
            tail->next = term(q->coeff, q->pow);
            tail = tail->next;
            q = q->next;
        }
    }
    if (q == NULL)
    {
        while (w)
        {
            tail->next = term(w->coeff, w->pow);
            tail = tail->next;
            w = w->next;
        }
    }
    return new;
}

//<------ SUBTRACTION OF TWO POLYNOMIAL FUNCTION ------->

a *subtraction(a *w, a *q)
{
    a *new = NULL;
    a *tail = NULL;
    a *qq = q;

    while (w && q)
    {
        if (w->pow == q->pow)
        {
            if (new == NULL)
            {
                new = term(w->coeff - q->coeff, w->pow);
                tail = new;
            }
            else
            {
                tail->next = term(w->coeff - q->coeff, w->pow);
                tail = tail->next;
            }
            w = w->next;
            q = q->next;
        }
        else if (w->pow > q->pow)
        {
            if (new == NULL)
            {
                new = term((w->coeff), w->pow);
                tail = new;
            }
            else
            {
                tail->next = term(w->coeff, w->pow);
                tail = tail->next;
            }
            w = w->next;
        }
        else
        {
            if (new == NULL)
            {
                new = term(-(q->coeff), q->pow);
                tail = new;
            }
            else
            {
                tail->next = term(-(q->coeff), q->pow);
                tail = tail->next;
            }
            q = q->next;
        }
    }
    if (w == NULL)
    {
        while (q)
        {
            tail->next = term(-(q->coeff), q->pow);
            tail = tail->next;
            q = q->next;
        }
    }
    if (q == NULL)
    {
        while (w)
        {
            tail->next = term(w->coeff, w->pow);
            tail = tail->next;
            w = w->next;
        }
    }
    return new;
}

//<------ MULTIPLICATION OF TWO POLYNOMIAL FUNCTION ------->

a *multiplication(a *w, a *q)
{
    a *nw = NULL;

    a *g = q;
    while (w)
    {
        while (g)
        {
            Make(&nw, (g->coeff) * (w->coeff), g->pow + w->pow);
            g = g->next;
        }
        w = w->next;
        g = q;
    }
    return nw;
}

//<------ DIVISION OF TWO POLYNOMIAL FUNCTION ------->

void division(a *bich, a *side, a **rem, a **ans)
{
    if (bich->pow < side->pow)
    {
        *rem = bich;
        return;
    }

    int t = bich->pow - side->pow;
    int x = (bich->coeff) / (side->coeff);

    a *www = side;
    while (www)
    {
        www->pow = www->pow + t;
        www->coeff = (www->coeff) * x;
        www = www->next;
    }
    www = side;
    print(www);

    a *result = subtraction(bich, www);

    print(result);

    while (result->coeff == 0)
    {
        if (result->next == NULL)
        {
            *rem = bich;
            result = NULL;
            return;
        }
        a *qq = result;
        result = result->next;
        free(qq);
    }
    printf("%p", result);

    while (www)
    {
        www->pow = www->pow - t;
        www->coeff = (www->coeff) / x;
        www = www->next;
    }
    www = side;
    Make(ans, x, t);
    if (result == NULL)
    {
        *rem = bich;
        return;
    }
    else
    {
        division(result, side, &(*rem), &(*ans));
    }
    printf("Hello World");
}

//<------ POLYNOMIAL EVOLUTION FUNCTION ------->
int evol(a *poly, int x)
{
    int a = 0;
    while (poly)
    {
        a = a + (poly->coeff) * pow(x, poly->pow);
        poly = poly->next;
    }
    return a;
}

//<------ MAIN FUNCTION ------->

int main()
{
    int i;
    int u = 1;
    int n, xx;
    do
    {
    //printf("\n\n|---------------------------------------------|");
    printf("\n\n|---------------------------------------------|\n");
    printf("| <--------BASIC CALCULATOR ------->          |\n");
    printf("|---------------------------------------------|\n");
    printf("\n");
    printf("|-------------------------------|\n");
    printf("|                               |\n");
    printf("| * Arithmetic Calculator *     |\n");
    printf("|                               |\n");
    printf("|                               |\n");
    printf("| <1.> ADDITION                 |\n");
    printf("| <2.> SUBTRACTION              |\n");
    printf("| <3.> MULTIPLICATION           |\n");
    printf("| <4.> DIVISION                 |\n");
    printf("|                               |\n");
    printf("|                               |\n");
    printf("| * Polynomial Calculator *     |\n");
    printf("|                               |\n");
    printf("|                               |\n");
    printf("| <5.> ADDITION                 |\n");
    printf("| <6.> SUBTRACTION              |\n");
    printf("| <7.> MULTIPLICATION           |\n");
    printf("| <8.> DIVISION                 |\n");
    printf("| <9.> POLYNOMIAL EVALUATION    |\n");
    printf("|-------------------------------|\n");

        printf("\nEnter your option : ");
        scanf("%d", &i);
        float gg, bb;
        a *head = NULL;
        a *head2 = NULL;
        a *result = NULL;
        if (i >= 1 && i < 5)
        {
            printf("Enter 1st number : ");
            scanf("%f", &gg);
            printf("Enter 2nd number : ");
            scanf("%f", &bb);
        }
        if (i > 4 && i < 8)
        {
            int n;
            printf("Enter number of terms in the polynomial 1 : ");
            scanf("%d", &n);
            head = createPoly(n);
            printf("Polynomial 1 is : ");
            print(head);
            int m;
            printf("Enter number of terms in the polynomial 2 : ");
            scanf("%d", &m);
            head2 = createPoly(m);
            printf("Polynomial 2 is : ");
            print(head2);
            
        }
        switch (i)
        {
        case 1:
            printf("Answer : %f\n", gg + bb);
            break;
        case 2:
            printf("Answer : %f\n", gg - bb);
            break;
        case 3:
            printf("Answer : %f\n", gg * bb);
            break;
        case 4:
            printf("Answer : %f\n", (float)gg / bb);
            break;
        case 5:
            result = sum(head, head2);
            printf("The sum of both polynomials is : ");
            print(result);
            break;
        case 6:
            printf("The subtraction of polynomials is : ");
            a *sub = subtraction(head, head2);
            print(sub);
            break;
        case 7:
            printf("The multiplication of polynomials is : ");
            a *sb = multiplication(head, head2);
            print(sb);
            break;
        case 8:
            // int n;
            printf("Enter number of terms in the polynomial 1 (Divisor) : ");
            scanf("%d", &n);
            head = createPoly(n);
            printf("Polynomial 1 is : ");
            print(head);
            int m;
            printf("Enter number of terms in the polynomial 2 (Dividend) : ");
            scanf("%d", &m);
            head2 = createPoly(m);
            printf("Polynomial 2 is : ");
            print(head2);
            printf("The division of polynomials is : \n");
            a *ans = NULL;
            a *rem = NULL;
            division(head2, head, &rem, &ans);
            if (ans == NULL)
            {
                printf("The quotient : 0\n");
            }
            else
            {
                printf("The quotient : ");
                print(ans);
            }
            printf("The remainder : ");
            print(rem);

        case 9:
            printf("Enter number of terms in the polynomial : ");
            scanf("%d", &n);
            a *pp = createPoly(n);
            printf("Your Polynomial is : ");
            print(pp);
            printf("Enter the value for which you want to find polynomial evaluation : ");
            scanf("%d", &xx);
            printf("The value of polynomial at x is : %d\n", evol(pp, xx));
            break;

        default:
            printf("Invalid Option\nPlease Enter a valid option.\n");
            break;
        }
        printf("Do you want to continue 1/0\n");
        scanf("%d", &u);
        if (u == 0)
        {
            printf("<< ** THANK YOU FOR USING OUR CALCULATOR ** >>\n");
        }
    } while (u);

    return 0;
}