Module Module1
    Const null As Integer = -1
    Public Structure Node
        Dim pointer As Integer
        Dim data As String
    End Structure

    Public Structure LinkedList
        Dim list() As Node
        Dim header, flp As Integer

        Sub init(ByVal size As Integer)
            header = null
            flp = 0
            ReDim list(size)
            For index = 0 To UBound(list) - 1
                list(index).data = ""
                list(index).pointer = index + 1
            Next
            ' assingning the last node of the list
            list(UBound(list)).data = ""
            list(UBound(list)).pointer = null
        End Sub
        Sub Insertion(ByVal dat As String)
            Dim tmp As Integer = flp
            list(flp).data = dat
            flp = list(flp).pointer
            list(tmp).pointer = null
            If header = null Then
                header = tmp
                Return
            End If
            If list(header).data > dat Then
                list(tmp).pointer = header
                header = tmp
                Return
            End If
            Dim cur, pre As Integer
            cur = header
            pre = null
            While cur <> null
                If list(cur).data > dat Then
                    Exit While
                Else
                    pre = cur
                    cur = list(cur).pointer
                End If
            End While
            Select Case cur
                Case null
                    list(pre).pointer = tmp
                    Return
                Case Else
                    list(pre).pointer = tmp
                    list(tmp).pointer = cur
                    Return
            End Select
        End Sub
        Sub Delete(ByVal dat As String)
            If list(header).pointer = null Then
                Return
            End If
            Dim tmp As Integer = header
            If list(header).data = dat Then
                header = list(header).pointer
                list(tmp).pointer = flp
                flp = tmp
                list(flp).data = ""
                Return
            End If
            Dim pre As Integer = null
            Dim cur As Integer = header
            While cur <> null
                If list(cur).data = dat Then
                    Exit While
                Else
                    pre = cur
                    cur = list(cur).pointer
                End If
            End While
            If cur = null Then
                Console.WriteLine("NO DATA")
                Return
            End If
            If list(cur).pointer = null Then
                list(cur).pointer = flp
                flp = cur
                list(pre).pointer = null
                Return
            End If
            list(pre).pointer = list(cur).pointer
            list(cur).pointer = flp
            flp = cur
            Return
        End Sub
        Sub print()
            Dim holder As Integer = header
            While holder <> null
                Console.WriteLine(list(holder).data)
                holder = list(holder).pointer
            End While
        End Sub
    End Structure
    Sub Main()
        Dim s As LinkedList
        Dim size As Integer
        Console.WriteLine("How many elements do you want?")
        size = Console.ReadLine - 1
        s.init(size)
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Dim dat As String
        For x = 0 To size
            Console.WriteLine("Enter {0} no. data", x + 1)
            dat = Console.ReadLine
            s.Insertion(dat)
        Next
        Console.Clear()
        s.print()
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("")
        Dim deletedat As String
        Console.WriteLine("what do you want to delete?")
        deletedat = Console.ReadLine()
        s.Delete(deletedat)
        Console.Clear()
        Dim add_dat As String
        Console.WriteLine("Adding")
        add_dat = Console.ReadLine
        s.Insertion(add_dat)
        s.print()
    End Sub
End Module