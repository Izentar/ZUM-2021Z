# ZUM-2021Z
Projekt z ZUM

Temat projektu:
Klasyfikacja (K). Konkretyzacja tego zadania wymaga:
1. ustalenia atrybutu dyskretnego reprezentującego pojęcie docelowe,
2. określenia zakresu przygotowania danych (np. przetworzenia do odpowiedniej postaci tabelarycznej, modyfikacji typów/zbiorów wartości atrybutów, eliminacji/naprawy defektów danych, modyfikacji rozkładu klas, losowania podzbiorów danych),
3. wskazania możliwości zdefiniowania nowych atrybutów,
4. wyboru algorytmu selekcji atrybutów,
5. wyboru algorytmów klasyfikacji,
6. wskazania parametrów algorytmów klasyfikacji wymagających strojenia (w tym parametrów umożliwiających zwiększanie wrażliwości na klasy rzadziej występujące, trudniejsze do prawidłowej predykcji lub o wyższych kosztach pomyłek)m
7. ustalenia procedur i kryteriów oceny jakości modeli (z uwzględnieniem rozkładu oraz, tam gdzie to uzasadnione, kosztów pomyłek).
Wybierając zbiór danych do tego zadania należy się upewnić, że występuje w nim co najmniej jeden atrybut dyskretny, który mógłby pełnić rolę interesującego pojęcia docelowego.


Zbiór danych:
Dane [Credit Card Fraud Detection](https://www.kaggle.com/mlg-ulb/creditcardfraud) (Kaggle) -- detekcja oszustw w płatnościach kartami.


Opis plików R:
* src/main.R - główne wywołanie funkcji. Należy po nim przechodzić i wywoływać odpowiednie dla użytkownika algorytmy
* src/fileProcessing.R - procesowanie pliku
* src/svm.R - algorytmy SMV-one-classification, SMV-nu-classification
* src/outForest.R - algorytm outForest, obecnie niedziałający
* src/runRRF.R - algorytm randomForest
* src/utils.R - pozostałe funkcje

Opis plików Pythona:
Wymagają one bezpośredniej modyfikacji w kodzie, aby uruchomić je z innymi parametrami. Są to proste skrypty pomocnicze, których zrobienie ich w R byłoby żmudne.
* analyzeLogs.py - skrypt do analizy logów. Zwraca najlepszy lub najgorszy eksperyment. Można do niego dodać inne funkcje decydujące o tym, który eksperment był lepszy.
* copyLines.py - skrypt kopiujący linie csv do nowego pliku. Podaje się tam maksymalną liczbę klasy 0, klasy 1 oraz pozycję klasy w pliku
* divideConfusion.py - skrypt naprawiający błędne zapisane logi pochodzące z randomForest.