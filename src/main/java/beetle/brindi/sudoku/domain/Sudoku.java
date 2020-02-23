package beetle.brindi.sudoku.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "sudoku")
public class Sudoku {

    @Id
    private Integer id;

    private String quizzes;

    private String solutions;

}
