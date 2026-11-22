package org.myworkflows.domain.ai;

import org.junit.jupiter.api.Test;
import org.myworkflows.exception.AiException;
import org.myworkflows.exception.AiException.Reason;

import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Mihai Surdeanu
 * @since 2.5
 */
public final class AiCallTest {

    @Test
    public void whenOnlyThePromptIsGivenThenTheRestIsLeftToTheDefaults() {
        // when
        final var call = AiCall.builder().prompt("hi").build();

        // then
        assertEquals("hi", call.prompt());
        assertNull(call.system());
        assertNull(call.model());
        assertNull(call.maxTokens());
        assertNull(call.effort());
        assertNull(call.timeout());
    }

    @Test
    public void whenEffortIsWrittenInAnyCaseThenItIsKeptInLowerCase() {
        assertEquals("xhigh", AiCall.builder().prompt("hi").effort(" XHigh ").build().effort());
    }

    @Test
    public void whenCallIsNotValidThenItIsRefused() {
        assertThrows(NullPointerException.class, () -> AiCall.builder().build());
        assertThrows(IllegalArgumentException.class, () -> AiCall.builder().prompt("hi").effort("extreme").build());
        assertThrows(IllegalArgumentException.class, () -> AiCall.builder().prompt("hi").maxTokens(0L).build());
        assertThrows(IllegalArgumentException.class, () -> AiCall.builder().prompt("hi").timeout(Duration.ZERO).build());
    }

    @Test
    public void whenAnswerStoppedAtTheLimitThenItIsTruncated() {
        assertTrue(new AiAnswer("partial", "claude-opus-5-5", 10, 16_000, AiAnswer.MAX_TOKENS).isTruncated());
        assertFalse(new AiAnswer("whole", "claude-opus-5-5", 10, 20, "end_turn").isTruncated());
    }

    @Test
    public void whenSchemaIsEmptyThenTheAnswerIsAText() {
        assertFalse(AiCall.builder().prompt("hi").schema(Map.of()).build().isStructured());
        assertNull(AiCall.builder().prompt("hi").schema(Map.of()).build().schema());
        assertTrue(AiCall.builder().prompt("hi").schema(Map.of("type", "object")).build().isStructured());
    }

    @Test
    public void whenAnswerIsHeldToASchemaThenItsOutputIsWhatItsJsonSays() {
        // given
        final var answer = new AiAnswer("{\"severity\":\"outage\",\"count\":2,\"tags\":[\"db\"]}", "claude-opus-5-5", 10, 20, "end_turn");

        // when
        final var output = answer.toOutput(true);

        // then
        assertEquals(Map.of("severity", "outage", "count", 2, "tags", List.of("db")), output);
        assertEquals(List.of("severity", "count", "tags"), List.copyOf(((Map<?, ?>) output).keySet()));
        assertEquals(answer.text(), answer.toOutput(false));
    }

    @Test
    public void whenAnswerCannotBeUsedThenItsOutputFails() {
        final var truncated = assertThrows(AiException.class,
            () -> new AiAnswer("{\"severity\":", "claude-opus-5-5", 10, 16_000, AiAnswer.MAX_TOKENS).toOutput(true));
        assertEquals(Reason.TRUNCATED, truncated.getReason());
        assertEquals("Claude reached the token limit of the call (maxTokens) before the end of its answer, after 16000 tokens",
            truncated.getMessage());
        assertEquals(Reason.TRUNCATED, assertThrows(AiException.class,
            () -> new AiAnswer("Half a", "claude-opus-5-5", 10, 16_000, AiAnswer.MAX_TOKENS).toOutput(false)).getReason());
        assertEquals(Reason.FAILED, assertThrows(AiException.class,
            () -> new AiAnswer("not json", "claude-opus-5-5", 10, 20, "end_turn").toOutput(true)).getReason());
    }

}
