package dot.render

import java.nio.file.{Paths, Path}

import dot.diagram.{Animation, Diagram}
import dot.graph.Graphs
import dot.svg.SvgGraphAnimation

sealed trait ExportFormat
case object JPG extends ExportFormat
case object PNG extends ExportFormat

/**
 * This class provides functionality for rendering diagrams and animations
 *
 * It can be used in two ways:
 *  - conventionally, via the `render` methods;
 *  - with special syntax sugar, allowing `render` calls on the diagrams/animations themselves.
 *
 * Usage examples:
 * {{{
 *   import dot.diagram.Diagram
 *   import java.nio.file.Paths
 *
 *   val renderer = Renderer(
 *     renderingOptions = RenderingOptions(density = 75),
 *     directory = Paths.get("images", "usage"),
 *     format = JPG
 *   )
 *
 *   // Conventional usage
 *   renderer
 *     .tweakRendering(_.withVerticalSpacing(2))
 *     .render("list", Diagram(List(1)))
 *
 *   // Sweet sugar, recommended
 *   import renderer._
 *   Diagram(List(1))
 *     .render("list", _.withVerticalSpacing(2))
 * }}}
 */
case class Renderer(
  renderingOptions: RenderingOptions = RenderingOptions(),
  animationOptions: AnimationOptions = AnimationOptions(),
  directory: Path = Paths.get("."),
  format: ExportFormat = PNG
) { self ⇒
  /** Tweak the rendering options with the provided funciton */
  def tweakRendering(tweak: RenderingOptions ⇒ RenderingOptions) =
    copy(renderingOptions = tweak(renderingOptions))

  /** Tweak the animation options with the provided funciton */
  def tweakAnimation(tweak: AnimationOptions ⇒ AnimationOptions) =
    copy(animationOptions = tweak(animationOptions))

  /** Render a diagram to a file with the given name (do not include the extension) */
  def render(name: String, diagram: Diagram): Unit = {
    val graph = Graphs.graph(renderingOptions)(diagram)
    format match {
      case PNG => PngRenderer.renderPng(
        graph,
        directory.resolve(s"$name.png"),
        renderingOptions)
      case JPG => JpgRenderer.renderJpg(
        graph,
        directory.resolve(s"$name.jpg"),
        renderingOptions)
    }
  }

  /** Render an animation to a GIF file with the given name (do not include the extension) */
  def render(name: String, animation: Animation): Unit = {
    val graphs = Graphs.graphs(renderingOptions, animationOptions.onionSkinLayers)(animation)
    AnimatedGifRenderer.renderAnimatedGif(
      graphs,
      SvgGraphAnimation.animate(animationOptions.interpolationFrames),
      directory.resolve(s"$name.gif"),
      renderingOptions,
      animationOptions
    )
  }

  /** Syntactic sugar for diagrams */
  implicit class DiagramRenderSyntax(diagram: Diagram) {
    def render(
      name: String,
      tweak: RenderingOptions ⇒ RenderingOptions = identity
    ) = self
      .tweakRendering(tweak)
      .render(name, diagram)
  }

  /** Syntactic sugar for animations */
  implicit class AnimationRenderSyntax(animation: Animation) {
    def render(
      name: String,
      tweakRendering: RenderingOptions ⇒ RenderingOptions = identity,
      tweakAnimation: AnimationOptions ⇒ AnimationOptions = identity
    ) = self
      .tweakRendering(tweakRendering)
      .tweakAnimation(tweakAnimation)
      .render(name, animation)
  }
}
