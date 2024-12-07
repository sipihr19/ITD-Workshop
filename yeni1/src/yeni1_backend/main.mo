import Debug "mo:base/Debug";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Float "mo:base/Float";
import Option "mo:base/Option";

actor StudentManagementSystem {
    // Öğrenci veri yapısı
    type Student = {
        id: Nat;
        firstName: Text;
        lastName: Text;
        age: Nat;
        department: Text;
        email: Text;
        courses: [Course];
        grades: [Grade];
    };

    // Ders veri yapısı
    type Course = {
        code: Text;
        name: Text;
        credit: Nat;
    };

    // Not veri yapısı
    type Grade = {
        courseCode: Text;
        score: Float;
    };

    // Öğrenci durumu
    type StudentStatus = {
        #Active;
        #Graduated;
        #Suspended;
    };

    // Öğrencileri saklamak için HashMap
    let students = HashMap.HashMap<Nat, Student>(10, Nat.equal, Nat.hash);
    var nextStudentId : Nat = 1;

    // Yeni öğrenci kayıt fonksiyonu
    public func registerStudent(
        firstName: Text, 
        lastName: Text, 
        age: Nat, 
        department: Text, 
        email: Text
    ) : async Nat {
        let newStudent : Student = {
            id = nextStudentId;
            firstName = firstName;
            lastName = lastName;
            age = age;
            department = department;
            email = email;
            courses = [];
            grades = [];
        };

        students.put(nextStudentId, newStudent);
        Debug.print("Yeni öğrenci kaydedildi: " # firstName # " " # lastName);
        
        nextStudentId += 1;
        return nextStudentId - 1;
    };

    // Ders ekleme fonksiyonu
    public func addCourse(
        courseCode: Text,
        courseName: Text,
        credit: Nat
    ) : async Course {
        let newCourse : Course = {
            code = courseCode;
            name = courseName;
            credit = credit;
        };
        
        Debug.print("Yeni ders eklendi: " # courseName);
        return newCourse;
    };

    // Öğrenciye ders atama fonksiyonu
    public func assignCourseToStudent(
        studentId: Nat, 
        course: Course
    ) : async ?Student {
        switch (students.get(studentId)) {
            case (null) { 
                Debug.print("Öğrenci bulunamadı");
                return null; 
            };
            case (?student) {
                let updatedCourses = Array.append(student.courses, [course]);
                
                let updatedStudent : Student = {
                    id = student.id;
                    firstName = student.firstName;
                    lastName = student.lastName;
                    age = student.age;
                    department = student.department;
                    email = student.email;
                    courses = updatedCourses;
                    grades = student.grades;
                };

                students.put(studentId, updatedStudent);
                Debug.print("Öğrenciye ders atandı: " # course.name);
                return ?updatedStudent;
            };
        };
    };

    // Not ekleme fonksiyonu
    public func addGrade(
        studentId: Nat, 
        courseCode: Text, 
        score: Float
    ) : async ?Student {
        switch (students.get(studentId)) {
            case (null) { 
                Debug.print("Öğrenci bulunamadı");
                return null; 
            };
            case (?student) {
                let newGrade : Grade = {
                    courseCode = courseCode;
                    score = score;
                };
                
                let updatedGrades = Array.append(student.grades, [newGrade]);
                
                let updatedStudent : Student = {
                    id = student.id;
                    firstName = student.firstName;
                    lastName = student.lastName;
                    age = student.age;
                    department = student.department;
                    email = student.email;
                    courses = student.courses;
                    grades = updatedGrades;
                };

                students.put(studentId, updatedStudent);
                Debug.print("Öğrenciye not eklendi: " # courseCode # " - " # Float.toText(score));
                return ?updatedStudent;
            };
        };
    };

    // Öğrencinin not ortalamasını hesaplama
    public query func calculateGPA(studentId: Nat) : async ?Float {
        switch (students.get(studentId)) {
            case (null) { 
                Debug.print("Öğrenci bulunamadı");
                return null; 
            };
            case (?student) {
                if (student.grades.size() == 0) {
                    return ?0.0;
                };

                let totalScore = Array.foldl<Grade, Float>(
                    func(acc, grade) { acc + grade.score }, 
                    0.0, 
                    student.grades
                );

                let gpa = totalScore / Float.fromInt(student.grades.size());
                return ?gpa;
            };
        };
    };

    // Tüm öğrencileri listeleme
    public query func listAllStudents() : async [Student] {
        let studentList = Array<Student>([]);
        
        for (student in students.vals()) {
            studentList := Array.append(studentList, [student]);
        };

        return studentList;
    };

    // Öğrenci bilgilerini güncelleme
    public func updateStudentInfo(
        studentId: Nat,
        firstName: ?Text,
        lastName: ?Text,
        age: ?Nat,
        department: ?Text,
        email: ?Text
    ) : async ?Student {
        switch (students.get(studentId)) {
            case (null) { 
                Debug.print("Öğrenci bulunamadı");
                return null; 
            };
            case (?student) {
                let updatedStudent : Student = {
                    id = student.id;
                    firstName = Option.unwrap(firstName, student.firstName);
                    lastName = Option.unwrap(lastName, student.lastName);
                    age = Option.unwrap(age, student.age);
                    department = Option.unwrap(department, student.department);
                    email = Option.unwrap(email, student.email);
                    courses = student.courses;
                    grades = student.grades;
                };

                students.put(studentId, updatedStudent);
                Debug.print("Öğrenci bilgileri güncellendi: " # updatedStudent.firstName # " " # updatedStudent.lastName);
                return ?updatedStudent;
            };
        };
    };

    // Öğrenci silme fonksiyonu
    public func deleteStudent(studentId: Nat) : async Bool {
        switch (students.get(studentId)) {
            case (null) { 
                Debug.print("Öğrenci bulunamadı");
                return false; 
            };
            case (?_) {
                students.delete(studentId);
                Debug.print("Öğrenci silindi: " # Nat.toText(studentId));
                return true;
            };
        };
    };
}
