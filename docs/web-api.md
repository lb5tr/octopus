# Protokół klient-serwer

  Serwer przyjmuje rządania od klientów i odpowiada.
  Rządania są zapisane jako prosty obiekt JSON:

  > {user-id: string, type : string, payload : value}

  Serwer również odpowiada korzystająć z JSON

  > {type: string, payload: value}

  Jeżeli type będzie równe "OK" to należy przejsć to przetwarzania
  danych z payload (rządanie się powiodło). Jeżeli coś poszło nie tak
  to type będzie równie "ERROR" a payload będzie obiektem:

  > {error-code: number, error-description: "string"}
  
  Użytkownicy łączą się z serwerem korzystając z URL'a

  > ws://foo.pl:7878/channel-manager

  Ten zasób (resource) jest managerem kanałów (entry-point
  dla każdego gracza). Tutaj można wylistować istniejące
  kanały/tworzyć nowe/dołączać do istniejących.

  Każdy kanał to osobny zasób WebSocket. Kanały rozróżnanie są
  na podstawie unikalnego ID.


## Komendy użytkownika dla channel-managera

### login
  
  Komenda sluży do logowania na serwerze, jako jedyna ignoruje user-id:

  >> {type: "login", payload: {user-name: "string", password: "sha1 hash"}}

  Serwer zwraca "OK" gdzie payload będzie stringiem z unikalnym dla tej sesji user-id
  lub "ERROR".

### list

  > {type : "list",
  > payload : null}
  
  Serwer odpowiada tablicą obiektów zawierającą wszsytkie
  istniejące na serwerze kanały. Obiekt opisujący kanał wygłada
  następująco:

  > { name: string,
  >   resource-locator: string,
  >   players-count: number,
  >   capactiy: number,
  >   map: string,
  >   protected: bool
  > }

  Przykładowa odpowiedź:
  > [
  >  {
  >    name: "foo",
  >    resource-locator: "ws://foo.pl:7878/1234-123-431",
  >    players-count: 5
  >    capactiy: 10,
  >    map: "warehouse",
  >    protected: false
  >  }
  > ]

  To żądanie nie zwraca błędów, co najwyżej pustą tablice kanałów.

### create

  > { type: "create", payload: 
  > {
  >  name:"string", 
  >  map: "string", 
  >  capacity: "string", 
  >  protected: bool, 
  >  password: "sha1 password hash string" 
  > }
  > }

  Serwer odpowiada "OK" gdzie payload będzie URL nowego kanłu np:
  > {type: "OK", payload: "ws://foo.bar:7878/123-123-432"}
 
  Lub "ERROR", kody błędów są opisanie w tablicy kodów błędów.
