using System;
using Occult;

namespace Shapes
{
	internal class PlotCoords : IComparable
	{
		private Vector v1;

		private Vector v2;

		private Vector v3;

		private Vector faceNormal;

		private int body = 1;

		public Vector V1
		{
			get
			{
				return v1;
			}
			set
			{
				v1 = value;
			}
		}

		public Vector V2
		{
			get
			{
				return v2;
			}
			set
			{
				v2 = value;
			}
		}

		public Vector V3
		{
			get
			{
				return v3;
			}
			set
			{
				v3 = value;
			}
		}

		public Vector FaceNormal
		{
			get
			{
				return faceNormal;
			}
			set
			{
				faceNormal = value;
			}
		}

		public int Body
		{
			get
			{
				return body;
			}
			set
			{
				body = value;
			}
		}

		public int CompareTo(object other)
		{
			return V2.Z.CompareTo(((PlotCoords)other).V2.Z);
		}
	}
}
