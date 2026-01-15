using System;

namespace Shapes
{
	internal class Vertex : IComparable
	{
		private Vertex v;

		private int vertexNum;

		public Vertex V
		{
			get
			{
				return v;
			}
			set
			{
				v = value;
			}
		}

		public int VertexNum
		{
			get
			{
				return vertexNum;
			}
			set
			{
				vertexNum = value;
			}
		}

		public int CompareTo(object other)
		{
			return VertexNum.CompareTo(((Vertex)other).VertexNum);
		}

		public override string ToString()
		{
			return V.ToString();
		}
	}
}
